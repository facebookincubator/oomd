// Copyright 2004-present Facebook. All Rights Reserved.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "oomd/include/Types.h"
#include "oomd/util/PluginArgParser.h"
#include "oomd/util/Util.h"

using namespace ::testing;

namespace Oomd {

TEST(ParseCGroup, testCgroupStringParsing) {
  PluginConstructionContext context("/root/cgroupfs");
  auto cgroups = PluginArgParser::parseCgroup(context, "cg1,cg2");
  std::unordered_set<CgroupPath> expected{
      {CgroupPath("/root/cgroupfs", "cg1")},
      {CgroupPath("/root/cgroupfs", "cg2")}};

  EXPECT_EQ(expected, cgroups);
}

TEST(ParseCGroup, testUnsignedIntParsing) {
  auto res = PluginArgParser::parseUnsignedInt("123");
  EXPECT_EQ(123, res);

  EXPECT_THROW(
      PluginArgParser::parseUnsignedInt("-123"), std::invalid_argument);
}

TEST(PluginArgParserTest, testPluginName) {
  PluginArgParser p("test_plugin");
  EXPECT_EQ("test_plugin", p.getName());

  p.setName("new_plugin_name");
  EXPECT_EQ("new_plugin_name", p.getName());
}

TEST(PluginArgParserTest, testArgParsingSuccess) {
  int64_t valueLongInt;
  int valueInt;
  double valueDouble;
  float valueFloat;
  bool valueBool;
  std::chrono::milliseconds valueMillis;
  ResourceType valueResourceType;
  std::vector<std::string> valueStrs;

  PluginArgParser p("test_plugin");

  std::function<std::vector<std::string>(const std::string&)> valueStrsFunc =
      [](const std::string& valueStr) -> std::vector<std::string> {
    return std::vector<std::string>{{valueStr + "-1", valueStr + "-2"}};
  };
  p.addArgument("arg_long_int", valueLongInt);
  p.addArgument("arg_int", valueInt);
  p.addArgument("arg_double", valueDouble);
  p.addArgument("arg_float", valueFloat);
  p.addArgument("arg_bool", valueBool);
  p.addArgument("arg_milli_second", valueMillis);
  p.addArgument("arg_resource_type", valueResourceType);
  p.addArgumentCustom("arg_strs", valueStrs, valueStrsFunc);

  // assert correct registered arg names
  std::unordered_set<std::string> expectedArgNames{
      {"arg_long_int"},
      {"arg_int"},
      {"arg_double"},
      {"arg_float"},
      {"arg_bool"},
      {"arg_milli_second"},
      {"arg_resource_type"},
      {"arg_strs"}};
  EXPECT_EQ(expectedArgNames, p.validArgNames());

  Engine::PluginArgs inputArgs{
      {"arg_long_int", "1234"},
      {"arg_int", "4321"},
      {"arg_double", "1.234"},
      {"arg_float", "4.321"},
      {"arg_bool", "true"},
      {"arg_milli_second", "456"},
      {"arg_resource_type", "io"},
      {"arg_strs", "some_str"}};

  auto res = p.parse(inputArgs);

  EXPECT_TRUE(res);

  EXPECT_EQ(1234, valueLongInt);
  EXPECT_EQ(4321, valueInt);
  EXPECT_EQ(1.234, valueDouble);
  EXPECT_EQ(4.321f, valueFloat);
  EXPECT_EQ(true, valueBool);
  EXPECT_EQ(std::chrono::milliseconds(456), valueMillis);
  EXPECT_EQ(ResourceType::IO, valueResourceType);

  std::vector<std::string> expectedStrs{{"some_str-1"}, {"some_str-2"}};
  EXPECT_EQ(expectedStrs, valueStrs);
}

TEST(PluginArgParserTest, testArgParsingSuccessOnResourceType) {
  ResourceType valueResourceTypeIO;
  ResourceType valueResourceTypeMem;

  PluginArgParser p("test_plugin");

  p.addArgument("arg_resource_type_io", valueResourceTypeIO);
  p.addArgument("arg_resource_type_mem", valueResourceTypeMem);

  // assert correct registered arg names
  std::unordered_set<std::string> expectedArgNames{
      {"arg_resource_type_io"}, {"arg_resource_type_mem"}};
  EXPECT_EQ(expectedArgNames, p.validArgNames());

  Engine::PluginArgs inputArgs{
      {"arg_resource_type_io", "io"}, {"arg_resource_type_mem", "memory"}};

  auto res = p.parse(inputArgs);

  EXPECT_TRUE(res);

  EXPECT_EQ(ResourceType::IO, valueResourceTypeIO);
  EXPECT_EQ(ResourceType::MEMORY, valueResourceTypeMem);
}

TEST(PluginArgParserTest, testArgParsingFailedOnInvalidResourceType) {
  ResourceType valueResourceType;

  PluginArgParser p("test_plugin");

  p.addArgument("arg_resource_type_io", valueResourceType);

  Engine::PluginArgs inputArgs{{"arg_resource_type_io", "kidding"}};

  auto res = p.parse(inputArgs);

  EXPECT_FALSE(res);
  EXPECT_THAT(
      std::string(res.error().what()),
      HasSubstr("Failed to parse argument \"arg_resource_type_io\", error"));
}

TEST(PluginArgParserTest, testArgParsingSuccessOnBool) {
  bool value1 = false;
  bool value2 = false;
  bool value3 = false;
  bool value4 = true;
  bool value5 = true;
  bool value6 = true;

  PluginArgParser p("test_plugin");

  p.addArgument("arg_1", value1);
  p.addArgument("arg_2", value2);
  p.addArgument("arg_3", value3);
  p.addArgument("arg_4", value4);
  p.addArgument("arg_5", value5);
  p.addArgument("arg_6", value6);

  Engine::PluginArgs inputArgs{
      {"arg_1", "true"},
      {"arg_2", "True"},
      {"arg_3", "1"},
      {"arg_4", "false"},
      {"arg_5", "False"},
      {"arg_6", "0"}};

  auto res = p.parse(inputArgs);

  EXPECT_TRUE(res);

  EXPECT_EQ(true, value1);
  EXPECT_EQ(true, value2);
  EXPECT_EQ(true, value3);
  EXPECT_EQ(false, value4);
  EXPECT_EQ(false, value5);
  EXPECT_EQ(false, value6);
}

TEST(PluginArgParserTest, testArgParsingFailedOnInvalidBool) {
  bool valueResourceType;

  PluginArgParser p("test_plugin");

  p.addArgument("arg_bool", valueResourceType);

  Engine::PluginArgs inputArgs{{"arg_bool", "kidding"}};

  auto res = p.parse(inputArgs);

  EXPECT_FALSE(res);
  EXPECT_THAT(
      std::string(res.error().what()),
      HasSubstr("Failed to parse argument \"arg_bool\", error"));
}

TEST(PluginArgParserTest, testArgParsingFailedWithUnkownArg) {
  int64_t valueLongInt;

  PluginArgParser p("test_plugin");

  p.addArgument("arg", valueLongInt);

  // assert correct registered arg names
  std::unordered_set<std::string> expectedArgNames{{"arg"}};
  EXPECT_EQ(expectedArgNames, p.validArgNames());

  Engine::PluginArgs inputArgs{{"arg", "1234"}, {"unknown_arg", "4321"}};

  auto res = p.parse(inputArgs);

  EXPECT_FALSE(res);
  EXPECT_THAT(
      std::string(res.error().what()),
      HasSubstr(
          "Unknown arg \"unknown_arg\" in plugin \"test_plugin\": Invalid argument"));
}

TEST(PluginArgParserTest, testArgParsingFailedWithMissingRequired) {
  int64_t valueLongInt;
  int64_t anotherLongInt;

  PluginArgParser p("test_plugin");

  p.addArgument("arg1", valueLongInt);
  p.addArgument("arg2", anotherLongInt, true);

  // assert correct registered arg names
  std::unordered_set<std::string> expectedArgNames{{"arg1"}, {"arg2"}};
  EXPECT_EQ(expectedArgNames, p.validArgNames());

  Engine::PluginArgs inputArgs{{"arg1", "1234"}};

  auto res = p.parse(inputArgs);

  EXPECT_FALSE(res);
  EXPECT_THAT(
      std::string(res.error().what()),
      HasSubstr(
          "Required arg \"arg2\" missing in plugin \"test_plugin\": Invalid argument"));
}

TEST(PluginArgParserTest, testArgParsingFailedWithInvalidValueStr) {
  int64_t valueLongInt;

  PluginArgParser p("test_plugin");

  p.addArgument("bad_arg", valueLongInt);

  // assert correct registered arg names
  std::unordered_set<std::string> expectedArgNames{{"bad_arg"}};
  EXPECT_EQ(expectedArgNames, p.validArgNames());

  Engine::PluginArgs inputArgs{{"bad_arg", "abcdefg"}};

  auto res = p.parse(inputArgs);

  EXPECT_FALSE(res);
  EXPECT_THAT(
      std::string(res.error().what()),
      HasSubstr("Failed to parse argument \"bad_arg\", error"));
}

} // namespace Oomd
