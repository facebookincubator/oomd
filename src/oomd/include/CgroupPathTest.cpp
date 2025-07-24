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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <unordered_map>

#include "oomd/include/CgroupPath.h"
#include "oomd/util/Fixture.h"

using namespace Oomd;

TEST(CgroupPathTest, ConstructorsTest) {
  CgroupPath path1("/sys/fs/cgroup", "myservice");
  CgroupPath path2(path1);
  EXPECT_EQ(path1.absolutePath(), path2.absolutePath());
  path1 = path2;
  EXPECT_EQ(path1.absolutePath(), path2.absolutePath());

  CgroupPath path3("/sys/fs/cgroup", "myservice");
  CgroupPath path4(std::move(path3));
  EXPECT_EQ(path4.absolutePath(), "/sys/fs/cgroup/myservice");
  CgroupPath path5 = std::move(path4);
  EXPECT_EQ(path5.absolutePath(), "/sys/fs/cgroup/myservice");

  // Test root token maps to root cgroup
  CgroupPath path("/sys/fs/cgroup", "/");
  EXPECT_TRUE(path.isRoot());
}

TEST(CgroupPathTest, GettersTest) {
  CgroupPath path("/sys/fs/cgroup", "system.slice/myservice.slice");
  EXPECT_EQ(path.absolutePath(), "/sys/fs/cgroup/system.slice/myservice.slice");
  EXPECT_EQ(path.relativePath(), "system.slice/myservice.slice");
  EXPECT_EQ(path.cgroupFs(), "/sys/fs/cgroup");
  ASSERT_EQ(path.relativePathParts().size(), 2);
  EXPECT_EQ(path.relativePathParts()[0], "system.slice");
  EXPECT_EQ(path.relativePathParts()[1], "myservice.slice");

  CgroupPath pathEmpty("/sys/fs/cgroup", "");
  EXPECT_EQ(pathEmpty.relativePath().size(), 0);
  EXPECT_EQ(pathEmpty.absolutePath(), "/sys/fs/cgroup");

  CgroupPath pathSame("/sys/fs/cgroup", "////////");
  EXPECT_EQ(pathSame.relativePath().size(), 0);
  EXPECT_EQ(pathSame.absolutePath(), "/sys/fs/cgroup");
}

TEST(CgroupPathTest, ModifiersTest) {
  CgroupPath path("/sys/fs/cgroup", "system.slice");
  path = path.getChild("myservice");
  path = path.getChild("/one/");
  path = path.getChild("two/");
  path = path.getChild("three");
  EXPECT_EQ(path.relativePath(), "system.slice/myservice/one/two/three");

  EXPECT_EQ(
      path.getParent().getParent().relativePath(),
      "system.slice/myservice/one");

  CgroupPath root("/sys/fs/cgroup", "");
  EXPECT_THROW(root.getParent(), std::invalid_argument);
}

TEST(CgroupPathTest, RelativeToTest) {
  CgroupPath path("/sys/fs/cgroup", "system.slice/myservice.slice");
  EXPECT_EQ(
      path.relativeTo("").absolutePath(),
      "/sys/fs/cgroup/system.slice/myservice.slice");
  EXPECT_EQ(
      path.relativeTo("/").absolutePath(),
      "/sys/fs/cgroup/system.slice/myservice.slice");
  EXPECT_EQ(
      path.relativeTo("machine@0001.slice").absolutePath(),
      "/sys/fs/cgroup/machine@0001.slice/system.slice/myservice.slice");
  EXPECT_EQ(
      path.relativeTo("machine@0001.slice/*").absolutePath(),
      "/sys/fs/cgroup/machine@0001.slice/*/system.slice/myservice.slice");
}

TEST(CgroupPathTest, ComparisonsTest) {
  CgroupPath path1("/sys/fs/cgroup", "system.slice");
  CgroupPath path2("/sys/fs/cgroup", "system.slice");
  EXPECT_EQ(path1, path2);

  CgroupPath path3("/sys/fs/cgroup", "asdf.slice");
  EXPECT_NE(path1, path3);

  EXPECT_FALSE(path3.isRoot());
  EXPECT_TRUE(path3.getParent().isRoot());
}

TEST(CgroupPathTest, HashTest) {
  std::unordered_map<CgroupPath, int> m;

  CgroupPath p1("/sys/fs/cgroup", "system.slice");
  CgroupPath p2("/sys/fs/cgroup", "workload.slice");
  m[p1] = 1;
  m[p2] = 2;

  EXPECT_EQ(m[p1], 1);
  EXPECT_EQ(m[p2], 2);

  CgroupPath p3("/sys/fs/cgroup", "system.slice");
  EXPECT_EQ(m[p3], 1);
}

TEST(CgroupPathTest, ResolveWildcardTest) {
  using F = Fixture;
  auto tempDir = F::mkdtempChecked();
  auto [name, fixture] = F::makeDir(
      "wildcard_root",
      {F::makeFile("a.txt", "content of a\n"),
       F::makeDir("b_dir", {}),
       F::makeFile("c.txt", "content of c\n"),
       F::makeDir(
           "d_dir",
           {
               F::makeFile("e.txt", "content of e\n"),
               F::makeDir("e_dir", {}),
           })});
  fixture.materialize(tempDir, name);

  CgroupPath p1(tempDir, "wildcard_root/**");
  auto resolved = p1.resolveWildcard();
  EXPECT_EQ(resolved.size(), 2);
  using ::testing::Contains;
  EXPECT_THAT(resolved, Contains(CgroupPath(tempDir, "wildcard_root/b_dir")));
  EXPECT_THAT(resolved, Contains(CgroupPath(tempDir, "wildcard_root/d_dir")));
}

TEST(CgroupPathTest, HasDescendantWithPrefixMatchingTest) {
  const std::string fs = "/cgroup2fs";
  auto matches = [&](const std::string& path, const std::string& pattern) {
    return CgroupPath(fs, path).hasDescendantWithPrefixMatching(
        CgroupPath(fs, pattern));
  };

  // basic pattern matching
  EXPECT_TRUE(matches("/foo/bar/baz", "/foo/*/baz"));

  // leading and trailing slashes are ignored
  EXPECT_TRUE(matches("foo/bar/baz", "/foo/"));
  EXPECT_TRUE(matches("foo/bar/baz", "foo/"));
  EXPECT_TRUE(matches("foo/bar/baz/", "foo/"));
  EXPECT_TRUE(matches("/foo/bar/baz/", "foo/"));
  EXPECT_TRUE(matches("/foo/bar/baz/", "foo/"));

  // patterns other than simple "*" per path segment are not supported
  EXPECT_FALSE(matches("foo/bar", "foo/b*"));
  EXPECT_TRUE(matches("foo/bar", "foo/*"));
  EXPECT_TRUE(matches("foo/bar", "foo/bar"));

  // paths which are parents of potential matches match
  EXPECT_TRUE(matches("foo/", "foo/bar/*/qoux/*"));
  EXPECT_TRUE(matches("foo/bar/baz", "foo/bar/*/qoux/*"));

  // star matches exactly one level of hierarchy
  EXPECT_FALSE(matches("foo/bar/baz/nux/qoux/muh", "foo/bar/*/qoux/*"));

  // paths which are not on the same lineage as any match do not match
  EXPECT_FALSE(matches("foo/nux", "foo/bar/*/qoux/*"));

  // paths which whose parents match pattern match pattern
  EXPECT_TRUE(matches("foo/bar/baz/qoux/nux/", "/foo/bar"));
  EXPECT_TRUE(matches("/qoux/nux/blah", "/*/nux"));
  EXPECT_TRUE(matches("/one/two/three", "/one/*"));

  // "/" matches everything
  EXPECT_TRUE(matches("/foo/bar/baz/", "/"));
  EXPECT_TRUE(matches("/foo/bar/baz/qoux/nux", "/"));
  EXPECT_TRUE(matches("/", "/"));
  EXPECT_TRUE(matches("/toplevel", "/"));
  EXPECT_TRUE(matches("toplevel/secondlevel", "/"));
  EXPECT_TRUE(matches("toplevel", "/"));
}
