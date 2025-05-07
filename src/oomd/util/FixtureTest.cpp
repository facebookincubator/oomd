// Copyright 2004-present Facebook. All Rights Reserved.

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <gtest/gtest.h>
#include <stdio.h>

#include <string>
#include <unordered_set>
#include <vector>

#include <oomd/util/Fixture.h>
#include <oomd/util/Util.h>

using namespace Oomd;

class FixtureTest : public ::testing::Test {
 protected:
  // remove all temp directories
  void TearDown() override {
    DIR* dir;
    std::string failureMsg;
    for (const auto& tempDir : tempDirs_) {
      dir = ::opendir(tempDir.c_str());
      if (dir != nullptr) {
        ::closedir(dir);
        // call `rm -r '$tempDir'` to remove recursively
        const auto& cmd = "rm -r '" + tempDir + "'";
        auto stream = ::popen(cmd.c_str(), "r");
        if (stream != nullptr) {
          ::pclose(stream);
        }
        dir = ::opendir(tempDir.c_str());
        if (dir != nullptr || errno != ENOENT) {
          failureMsg += " " + tempDir;
        }
      }
    }
    if (failureMsg != "") {
      FAIL() << "remove failed:" + failureMsg;
    }
  }

  std::unordered_set<std::string> tempDirs_;
};

TEST_F(FixtureTest, TestMkdtempChecked) {
  DIR* dir;
  std::string tempDir;

  for (int i = 0; i < 100; i++) {
    EXPECT_NO_THROW(tempDir = Fixture::mkdtempChecked());
    // directory exists
    EXPECT_TRUE((dir = ::opendir(tempDir.c_str())) && ::closedir(dir) == 0);
    // directory not already seen
    EXPECT_TRUE(tempDirs_.find(tempDir) == tempDirs_.end());
    tempDirs_.insert(tempDir);
  }
}

TEST_F(FixtureTest, TestMkdirsChecked) {
  DIR* dir;
  std::string tempDir;
  std::string path;

  EXPECT_NO_THROW(tempDir = Fixture::mkdtempChecked());
  tempDirs_.insert(tempDir);

  // nothing should happen
  EXPECT_NO_THROW(Fixture::mkdirsChecked(tempDir));

  // should ignore repeated slashes
  EXPECT_NO_THROW(Fixture::mkdirsChecked("A//B///C/", tempDir));
  path = tempDir + "/A/B/C";
  EXPECT_TRUE((dir = ::opendir(path.c_str())) && ::closedir(dir) == 0);

  // should create D without touching A
  EXPECT_NO_THROW(Fixture::mkdirsChecked("A/D", tempDir));
  path = tempDir + "/A/B/C";
  EXPECT_TRUE((dir = ::opendir(path.c_str())) && ::closedir(dir) == 0);
  path = tempDir + "/A/D";
  EXPECT_TRUE((dir = ::opendir(path.c_str())) && ::closedir(dir) == 0);
}

TEST_F(FixtureTest, TestWriteChecked) {
  std::vector<char> buf(1024);
  std::string tempDir;

  EXPECT_NO_THROW(tempDir = Fixture::mkdtempChecked());
  tempDirs_.insert(tempDir);
  const auto filepath = tempDir + "/writetest.txt";
  const std::string content = "Hello, Facebook!\nHello, world!\n";
  const std::string newContent = "Bye, Facebook!\nHello again, world!\n";

  // create and write
  EXPECT_NO_THROW(Fixture::writeChecked(filepath, content));
  int fd = ::open(filepath.c_str(), O_RDONLY);
  EXPECT_GE(fd, 0);
  int bytes_read = Util::readFull(fd, buf.data(), content.length());
  ::close(fd);
  EXPECT_EQ(bytes_read, content.length());
  EXPECT_EQ(std::string(buf.data(), content.length()), content);

  // overwrite
  EXPECT_NO_THROW(Fixture::writeChecked(filepath, newContent));
  fd = ::open(filepath.c_str(), O_RDONLY);
  EXPECT_GE(fd, 0);
  bytes_read = Util::readFull(fd, buf.data(), newContent.length());
  ::close(fd);
  EXPECT_EQ(bytes_read, newContent.length());
  EXPECT_EQ(std::string(buf.data(), newContent.length()), newContent);
}

TEST_F(FixtureTest, TestRmrChecked) {
  DIR* dir;
  std::string tempDir;

  EXPECT_NO_THROW({
    // create directory tree
    tempDir = Fixture::mkdtempChecked();
    tempDirs_.insert(tempDir);
    Fixture::mkdirsChecked("A/B/C", tempDir);
    Fixture::mkdirsChecked("D/E", tempDir);
    Fixture::writeChecked(tempDir + "/file1", "file1 content");
    Fixture::writeChecked(tempDir + "/A/file2", "file2 content");
    Fixture::writeChecked(tempDir + "/D/E/file3", "file3 content");
    Fixture::rmrChecked(tempDir);
  });

  tempDirs_.erase(tempDir);
  // check file does not exist
  dir = ::opendir(tempDir.c_str());
  if (dir != nullptr) {
    ::closedir(dir);
    FAIL() << "Failed to remove tempDir: " + tempDir;
  } else {
    EXPECT_TRUE(errno == ENOENT);
  }
}

bool verifyFile(const std::string& filepath, const std::string& content) {
  int fd = ::open(filepath.c_str(), O_RDONLY);
  if (fd < 0) {
    return false;
  }
  std::vector<char> buf(content.size());
  ssize_t count = Util::readFull(fd, buf.data(), content.size());
  ::close(fd);
  return std::string(buf.data(), count) == content;
}

TEST_F(FixtureTest, TestMaterialize) {
  DIR* dir;
  std::string tempDir;

  EXPECT_NO_THROW(tempDir = Fixture::mkdtempChecked());
  tempDirs_.insert(tempDir);
  const auto fixture = Fixture::makeDir(
      "root",
      {Fixture::makeDir(
           "A",
           {Fixture::makeDir("B", {Fixture::makeDir("C")}),
            Fixture::makeFile("file2", "file2 content")}),

       Fixture::makeDir(
           "D",
           {Fixture::makeDir(
               "E", {Fixture::makeFile("file3", "file3 content")})}),
       Fixture::makeFile("file1", "file1 content")});
  EXPECT_NO_THROW(fixture.second.materialize(tempDir, fixture.first));

  auto root = tempDir + "/" + "root";
  auto path = root + "/A/B/C";
  EXPECT_TRUE((dir = ::opendir(path.c_str())) && ::closedir(dir) == 0);
  path = root + "/A/file2";
  EXPECT_TRUE(verifyFile(path, "file2 content"));
  path = root + "/D/E/file3";
  EXPECT_TRUE(verifyFile(path, "file3 content"));
  path = root + "/file1";
  EXPECT_TRUE(verifyFile(path, "file1 content"));
}
