#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <cerrno>
#include <iostream>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sstream>
#include <fstream>
#include <vector>
#include <unordered_set>
#include <sys/stat.h> // Include this header for mkdir
#include <algorithm>
#include <regex>

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"

// Define SYS_process_madvise and SYS_pidfd_open if not defined
#ifndef SYS_process_madvise
#define SYS_process_madvise 443 // Update this if necessary for your architecture
#endif

#ifndef SYS_pidfd_open
#define SYS_pidfd_open 434 // Update this if necessary for your architecture
#endif

void pageInMemory(pid_t pid);

struct MemoryRegion
{
    unsigned long start;
    unsigned long end;
    size_t swapSize;
};

std::vector<MemoryRegion> getSwappedRegions(pid_t pid)
{
    std::vector<MemoryRegion> regions;
    std::unordered_set<unsigned long> seenAddresses;

    std::string smapsPath = "/proc/" + std::to_string(pid) + "/smaps";
    std::ifstream smapsFile(smapsPath);
    if (!smapsFile.is_open())
    {
        std::cerr << "Failed to open " << smapsPath << std::endl;
        return regions;
    }

    std::string line;
    MemoryRegion currentRegion = {0, 0, 0};
    while (std::getline(smapsFile, line))
    {
        try
        {
            if (line.find("Swap:") != std::string::npos)
            {
                // Extract the last token which should be the swap size
                std::istringstream iss(line);
                std::string key, swapSizeStr;
                iss >> key >> swapSizeStr; // "Swap:" and the swap size
                size_t swapSize = std::stoul(swapSizeStr);
                currentRegion.swapSize = swapSize;
                if (swapSize > 0)
                {
                    if (seenAddresses.find(currentRegion.start) == seenAddresses.end())
                    {
                        regions.push_back(currentRegion);
                        seenAddresses.insert(currentRegion.start);
                    }
                }
            }
            else if (line.find('-') != std::string::npos)
            {
                size_t pos = line.find('-');
                currentRegion.start = std::stoul(line.substr(0, pos), nullptr, 16);
                currentRegion.end = std::stoul(line.substr(pos + 1, line.find(' ') - pos - 1), nullptr, 16);
                currentRegion.swapSize = 0;
            }
        }
        catch (const std::invalid_argument &e)
        {
            std::cerr << "Invalid argument in line: " << line << std::endl;
        }
        catch (const std::out_of_range &e)
        {
            std::cerr << "Out of range error in line: " << line << std::endl;
        }
    }

    smapsFile.close();
    return regions;
}

void pageInMemory(pid_t pid)
{
    // Get the swapped memory regions
    std::vector<MemoryRegion> regions = getSwappedRegions(pid);

    // Sort the regions by swap size in descending order
    std::sort(regions.begin(), regions.end(), [](const MemoryRegion &a, const MemoryRegion &b)
              { return a.swapSize > b.swapSize; });

    // Page in the memory regions
    for (const auto &region : regions)
    {
        std::string memFilePath = "/proc/" + std::to_string(pid) + "/mem";
        int memFile = open(memFilePath.c_str(), O_RDONLY);
        if (memFile == -1)
        {
            std::cerr << "Failed to open " << memFilePath << ": " << strerror(errno) << std::endl;
            return;
        }
        size_t length = region.end - region.start;
        std::vector<char> buffer(length);

        if (pread(memFile, buffer.data(), length, region.start) == -1)
        {
            std::cerr << "Failed to read memory region " << std::hex << region.start << "-" << region.end << ": " << strerror(errno) << std::endl;
        }
        else
        {
            std::cout << "Memory region " << std::hex << region.start << "-" << region.end << " read successfully." << std::endl;
            // Process the memory content as needed
        }

        close(memFile);
    }
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <pid>\n", argv[0]);
        return EXIT_FAILURE;
    }
    int pid = atoi(argv[1]);
    pageInMemory(pid);
    return EXIT_SUCCESS;
}