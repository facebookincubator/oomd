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

#include "oomd/fixtures/FsFixture.h"

namespace Oomd {

constexpr auto kDataMeminfo = R"(MemTotal:       58616708 kB
MemFree:         2955848 kB
MemAvailable:   49878948 kB
Buffers:         4970160 kB
Cached:         35172508 kB
SwapCached:            0 kB
Active:         27822776 kB
Inactive:       19032824 kB
Active(anon):    4024064 kB
Inactive(anon):  2822876 kB
Active(file):   23798712 kB
Inactive(file): 16209948 kB
Unevictable:       11684 kB
Mlocked:           11668 kB
SwapTotal:       2097148 kB
SwapFree:        1097041 kB
Dirty:             24576 kB
Writeback:             0 kB
AnonPages:       6725380 kB
Mapped:           657992 kB
Shmem:            130124 kB
Slab:            8238080 kB
SReclaimable:    7334616 kB
SUnreclaim:       903464 kB
KernelStack:       45984 kB
PageTables:        75756 kB
NFS_Unstable:          0 kB
Bounce:                0 kB
WritebackTmp:          0 kB
CommitLimit:    31405500 kB
Committed_AS:   21265672 kB
VmallocTotal:   34359738367 kB
VmallocUsed:           0 kB
VmallocChunk:          0 kB
HardwareCorrupted:     0 kB
AnonHugePages:     71680 kB
ShmemHugePages:        0 kB
ShmemPmdMapped:        0 kB
CmaTotal:              0 kB
CmaFree:               0 kB
HugePages_Total:       0
HugePages_Free:        0
HugePages_Rsvd:        0
HugePages_Surp:        0
Hugepagesize:       2048 kB
Hugetlb:               0 kB
DirectMap4k:     4544368 kB
DirectMap2M:    53127168 kB
DirectMap1G:     4194304 kB
)";

constexpr auto kDataMounts =
    R"(sysfs /sys sysfs rw,seclabel,nosuid,nodev,noexec,relatime 0 0
proc /proc proc rw,nosuid,nodev,noexec,relatime 0 0
devtmpfs /dev devtmpfs rw,seclabel,nosuid,size=58710812k,nr_inodes=14677703,mode=755 0 0
securityfs /sys/kernel/security securityfs rw,nosuid,nodev,noexec,relatime 0 0
tmpfs /dev/shm tmpfs rw,seclabel,nosuid,nodev 0 0
devpts /dev/pts devpts rw,seclabel,nosuid,noexec,relatime,gid=5,mode=620,ptmxmode=000 0 0
tmpfs /run tmpfs rw,seclabel,nosuid,nodev,mode=755 0 0
cgroup2 /sys/fs/cgroup cgroup2 rw,nosuid,nodev,noexec,relatime,nsdelegate 0 0
pstore /sys/fs/pstore pstore rw,seclabel,nosuid,nodev,noexec,relatime 0 0
bpf /sys/fs/bpf bpf rw,relatime,mode=700 0 0
configfs /sys/kernel/config configfs rw,relatime 0 0
/dev/vda3 / btrfs rw,seclabel,relatime,compress-force=zstd,discard,space_cache,subvolid=5,subvol=/ 0 0
selinuxfs /sys/fs/selinux selinuxfs rw,relatime 0 0
systemd-1 /proc/sys/fs/binfmt_misc autofs rw,relatime,fd=26,pgrp=1,timeout=0,minproto=5,maxproto=5,direct 0 0
hugetlbfs /dev/hugepages hugetlbfs rw,seclabel,relatime,pagesize=2M 0 0
debugfs /sys/kernel/debug debugfs rw,seclabel,relatime,mode=755 0 0
mqueue /dev/mqueue mqueue rw,seclabel,relatime 0 0
sunrpc /var/lib/nfs/rpc_pipefs rpc_pipefs rw,relatime 0 0
nfsd /proc/fs/nfsd nfsd rw,relatime 0 0
fusectl /sys/fs/fuse/connections fusectl rw,relatime 0 0
/dev/vda1 /boot ext4 rw,seclabel,relatime,data=ordered 0 0
none-tw-channel /run/facebook/tw_logging_channel tmpfs rw,seclabel,relatime,size=10240k,nr_inodes=4096 0 0
tracefs /sys/kernel/debug/tracing tracefs rw,seclabel,relatime 0 0
/etc/auto.home.override /home autofs rw,relatime,fd=5,pgrp=2182594,timeout=600,minproto=5,maxproto=5,indirect 0 0
binfmt_misc /proc/sys/fs/binfmt_misc binfmt_misc rw,relatime 0 0
tmpfs /run/user/30015 tmpfs rw,seclabel,nosuid,nodev,relatime,size=11744436k,mode=700,uid=30015,gid=30015 0 0
tmpfs /run/user/0 tmpfs rw,seclabel,nosuid,nodev,relatime,size=11744436k,mode=700 0 0
tmpfs /run/user/180401 tmpfs rw,seclabel,nosuid,nodev,relatime,size=11744436k,mode=700,uid=180401,gid=100 0 0
)";

constexpr auto kDataCgMemStat = R"(anon 1294168064
file 3870687232
kernel_stack 7225344
slab 296456192
sock 1228800
shmem 135168
file_mapped 199778304
file_dirty 405504
file_writeback 811008
inactive_anon 296529920
active_anon 997982208
inactive_file 1284907008
active_file 2586988544
unevictable 28672
slab_reclaimable 262471680
slab_unreclaimable 33984512
pgfault 734452455
pgmajfault 4627590
pgrefill 243154
pgscan 787288
pgsteal 770913
pgactivate 13992
pgdeactivate 233504
pglazyfree 0
pglazyfreed 0
workingset_refault 510510
workingset_activate 25608
workingset_restore 3927
workingset_nodereclaim 0
)";

constexpr auto kDataVmstat = R"(first_key 12345
second_key 678910
thirdkey 999999
)";

namespace {

using F = Fixture;

const auto kEntCgroup = F::makeDir(
    "cgroup",
    {F::makeDir(
        "system.slice",
        {
            F::makeDir(
                "service1.service",
                {F::makeFile(
                    "cgroup.procs",
                    "456\n"
                    "789\n")}),
            F::makeDir(
                "service2.service",
                {F::makeFile(
                    "memory.pressure",
                    "aggr 128544748770\n"
                    "some 1.11 2.22 3.33\n"
                    "full 4.44 5.55 6.66\n")}),
            F::makeDir(
                "service3.service",
                {
                    F::makeFile(
                        "memory.pressure",
                        "aggr 128544748770\n"
                        "some 1.11 2.22 3.33\n"
                        "full 4.44 5.55 6.66\n"
                        "0 0 0\n"
                        "0 0 0\n"
                        "0 0 0\n"
                        "0 0 0\n"),
                    F::makeFile(
                        "cgroup.events",
                        "populated 0\n"
                        "frozen 0\n"),
                }),
            F::makeDir(
                "slice1.slice",
                {F::makeDir(
                     "service1.service",
                     {F::makeFile(
                         "cgroup.procs",
                         "456\n"
                         "789\n")}),
                 F::makeDir(
                     "service2.service",
                     {F::makeFile(
                         "cgroup.procs",
                         "12\n"
                         "34\n")})}),
            F::makeFile("cgroup.controllers", "cpu io memory pids\n"),
            F::makeFile("cgroup.procs", "123\n"),
            F::makeFile(
                "cgroup.events",
                "populated 1\n"
                "frozen 0\n"),
            F::makeFile(
                "cgroup.stat",
                "nr_descendants 34\n"
                "nr_dying_descendants 27\n"),
            F::makeFile(
                "io.pressure",
                "some avg10=1.12 avg60=2.23 avg300=3.34 total=134829384401\n"
                "full avg10=4.45 avg60=5.56 avg300=6.67 total=128544748771\n"),
            F::makeFile(
                "io.stat",
                "1:10 rbytes=1111111 wbytes=2222222 rios=33 wios=44 dbytes=5555555555 dios=6\n"
                "1:11 rbytes=2222222 wbytes=3333333 rios=44 wios=55 dbytes=6666666666 dios=7\n"),
            F::makeFile("memory.current", "987654321\n"),
            F::makeFile("memory.max", "654\n"),
            F::makeFile("memory.high", "1000\n"),
            F::makeFile("memory.high.tmp", "2000 20000\n"),
            F::makeFile("memory.low", "333333\n"),
            F::makeFile("memory.min", "666\n"),
            F::makeFile(
                "memory.pressure",
                "some avg10=1.11 avg60=2.22 avg300=3.33 total=134829384400\n"
                "full avg10=4.44 avg60=5.55 avg300=6.66 total=128544748770\n"),
            F::makeFile("memory.stat", kDataCgMemStat),
            F::makeFile("memory.swap.current", "321321\n"),
        })});

const auto kEntFsData = F::makeDir(
    "fs_data",
    {
        F::makeDir(
            "dir1",
            {
                F::makeFile(
                    "stuff",
                    "hello world\n"
                    "my good man\n"
                    "\n"
                    "1\n"),
            }),
        F::makeDir("dir2", {F::makeFile("empty")}),
        F::makeDir("dir3", {F::makeFile("empty")}),
        F::makeDir(
            "wildcard",
            {
                F::makeDir("dir1", {F::makeFile("file")}),
                F::makeDir("dir2", {F::makeFile("file")}),
                F::makeDir("different_dir", {F::makeFile("file")}),
                F::makeFile("file"),
            }),
        F::makeFile("file1"),
        F::makeFile("file2"),
        F::makeFile("file3"),
        F::makeFile("file4"),
    });

const auto kEntProc = F::makeDir(
    "proc",
    {
        F::makeFile("meminfo", kDataMeminfo),
        F::makeFile("mounts", kDataMounts),
        F::makeFile("vmstat", kDataVmstat),
    });

const auto kEntSysDevBlock = F::makeDir(
    "sys_dev_block",
    {
        F::makeDir(
            "1:0",
            {F::makeDir("queue", {F::makeFile("rotational", "0\n")})}),
        F::makeDir(
            "1:1",
            {F::makeDir("queue", {F::makeFile("rotational", "1\n")})}),
        F::makeDir(
            "1:2",
            {F::makeDir("queue", {F::makeFile("rotational", "\n")})}),
    });

const auto kEntFsFixture = F::makeDir(
    "fs_test",
    {
        kEntCgroup,
        kEntFsData,
        kEntProc,
        kEntSysDevBlock,
    });

} // namespace

constexpr auto kCgroupDataDir = "fs_test/cgroup/system.slice";
constexpr auto kFsDataDir = "fs_test/fs_data";
constexpr auto kFsVmstatFile = "fs_test/proc/vmstat";
constexpr auto kFsMeminfoFile = "fs_test/proc/meminfo";
constexpr auto kFsMountsFile = "fs_test/proc/mounts";
constexpr auto kFsDeviceDir = "fs_test/sys_dev_block";

void FsFixture::materialize() {
  tempFixtureDir_ = F::mkdtempChecked();
  kEntFsFixture.second.materialize(tempFixtureDir_, kEntFsFixture.first);
}

void FsFixture::teardown() {
  F::rmrChecked(tempFixtureDir_);
  tempFixtureDir_ = "";
}

std::string FsFixture::cgroupDataDir() {
  return tempFixtureDir_ + "/" + kCgroupDataDir;
}
std::string FsFixture::fsDataDir() {
  return tempFixtureDir_ + "/" + kFsDataDir;
}
std::string FsFixture::fsVmstatFile() {
  return tempFixtureDir_ + "/" + kFsVmstatFile;
}
std::string FsFixture::fsMeminfoFile() {
  return tempFixtureDir_ + "/" + kFsMeminfoFile;
}
std::string FsFixture::fsMountsFile() {
  return tempFixtureDir_ + "/" + kFsMountsFile;
}
std::string FsFixture::fsDeviceDir() {
  return tempFixtureDir_ + "/" + kFsDeviceDir;
}
} // namespace Oomd
