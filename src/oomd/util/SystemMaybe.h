/*
 * Copyright (C) 2018-present, Facebook, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <sstream>
#include <variant>

// This header provides SystemMaybe<Value> - a type which holds either
// Value or a std::system_error intended to be used for return values
// of functions that can have errors.

namespace Oomd {

// A Unit type to represent no value (void isn't able to be a value,
// so use this instead as SystemMaybe<Unit> instead of
// SystemMaybe<void>
struct Unit {};

// This is an error type implicitly convertible to SystemMaybe so
// that we can create errors without specifying the Value type and
// then they are converted at return
class SystemError final {
  std::system_error err_;

 public:
  SystemError() = default;
  SystemError(const SystemError&) = default;
  SystemError(SystemError&&) = default;
  SystemError& operator=(const SystemError&) = default;
  SystemError& operator=(SystemError&&) = default;

  SystemError(const std::system_error& err) : err_(err) {}
  SystemError(std::system_error&& err) : err_(std::move(err)) {}

  std::system_error& error() & {
    return err_;
  }

  const std::system_error& error() const& {
    return err_;
  }

  std::system_error&& error() && {
    return std::move(err_);
  }
};

template <class Value>
class SystemMaybe final {
  using base_type = std::variant<Value, std::system_error>;
  base_type base_;

 public:
  SystemMaybe() = default;
  SystemMaybe(const SystemMaybe& other) = default;
  SystemMaybe(SystemMaybe&& other) = default;
  ~SystemMaybe() = default;

  /*
   * Assignment operators
   */
  SystemMaybe& operator=(const SystemMaybe& that) = default;
  SystemMaybe& operator=(SystemMaybe&& that) = default;

  // Value constructors - allows for implicit conversion to SystemMaybe
  template <
      class Dummy = Value,
      typename = std::enable_if_t<std::is_copy_constructible<Dummy>::value>>
  SystemMaybe(const Value& val) noexcept(noexcept(Value(val)))
      : base_(std::in_place_index_t<0>{}, val) {}
  template <
      class Dummy = Value,
      typename = std::enable_if_t<std::is_move_constructible<Dummy>::value>>
  SystemMaybe(Value&& val) noexcept(noexcept(Value(std::move(val))))
      : base_(std::in_place_index_t<0>{}, std::move(val)) {}

  // Implicit conversions from the SystemError type
  SystemMaybe(const SystemError& err)
      : base_(std::in_place_index_t<1>{}, err.error()) {}
  SystemMaybe(SystemError&& err)
      : base_(std::in_place_index_t<1>{}, std::move(err.error())) {}

  SystemMaybe& operator=(const SystemError& err) {
    base_ = base_type(std::in_place_index_t<1>{}, err.error());
    return *this;
  }

  SystemMaybe& operator=(SystemError&& err) {
    base_ = base_type(std::in_place_index_t<1>{}, std::move(err.error()));
    return *this;
  }

  // Everything else is just various accessors for Value or error

  constexpr const Value& value() const& {
    return std::get<0>(base_);
  }

  constexpr Value& value() & {
    return std::get<0>(base_);
  }

  constexpr Value& value() && {
    return std::get<0>(std::move(base_));
  }

  constexpr const std::system_error& error() const& {
    return std::get<1>(base_);
  }

  constexpr std::system_error& error() & {
    return std::get<1>(base_);
  }

  constexpr std::system_error&& error() && {
    return std::get<1>(std::move(base_));
  }

  constexpr operator bool() const noexcept {
    return !base_.index();
  }

  constexpr const Value& operator*() const& {
    return this->value();
  }

  constexpr Value& operator*() & {
    return this->value();
  }

  constexpr Value&& operator*() && {
    return std::move(this->value());
  }

  constexpr const Value* operator->() const {
    return std::addressof(this->value());
  }

  constexpr Value* operator->() {
    return std::addressof(this->value());
  }
};

// This all provides a method concatStrs which concatenates a
// variadic number of arguments into a single string. This supports
// any type that supports output to a stringstream
namespace detail {
template <typename... Strs>
std::string concatStrs(Strs&&... strs) {
  std::ostringstream oss;
  (oss << ... << std::forward<Strs>(strs));
  return oss.str();
}
} // namespace detail

// These functions are likely to be the most common way to construct errors -
// just pass in the error code and the rest of the arguments will be
// concatenated into a string for the message
template <typename... Msg>
auto systemError(int err, Msg&&... msg) {
  return SystemError(std::system_error(
      err,
      std::system_category(),
      detail::concatStrs(std::forward<Msg>(msg)...)));
}

template <typename... Msg>
auto systemError(std::error_code ec, Msg&&... msg) {
  return SystemError(
      std::system_error(ec, detail::concatStrs(std::forward<Msg>(msg)...)));
}

// This allows for error "chaining" to add additional context to an error
template <typename... Msg>
auto systemError(std::system_error err, Msg&&... msg) {
  std::string w = err.what();
  w += " -- ";
  w += detail::concatStrs(std::forward<Msg>(msg)...);
  return SystemError(std::system_error(err.code(), std::move(w)));
}

#define SYSTEM_ERROR(c, ...) \
  ::Oomd::systemError(c, "[", __FILE__, ":", __LINE__, "] ", ##__VA_ARGS__)

namespace {
[[maybe_unused]] auto noSystemError() {
  return SystemMaybe<Unit>();
}
} // namespace

#define ASSERT_SYS_OK(maybe)            \
  ({                                    \
    auto x = (maybe);                   \
    ASSERT_TRUE(x) << x.error().what(); \
    std::move(*x);                      \
  })

} // namespace Oomd
