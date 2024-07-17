#include <iostream>
#include <vector>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <linux/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sstream>
#ifndef SYS_pidfd_open
#define SYS_pidfd_open 434 // Update this if necessary for your architecture
#endif

#define LOG_VICTIM std::cout << "victim: " 
#define ERROR_VICTIM std::cerr << "victim error: "

// Wrapper for pidfd_open syscall
int pidfd_open(pid_t pid, unsigned int flags)
{
    return syscall(SYS_pidfd_open, pid, flags);
}

// Define a chunk size (e.g., 100 MB)
constexpr size_t CHUNK_SIZE = 100 * 1024 * 1024; // 100 MB

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        ERROR_VICTIM << "Usage: " << argv[0] << " <total_memory_to_allocate_in_MB>" << " <time_to_sleep_in_seconds>" << std::endl;
        return EXIT_FAILURE;
    }

    // Parse the total memory to allocate from command line arguments
    size_t total_memory_to_allocate = std::stoull(argv[1]) * 1024 * 1024; // Convert MB to bytes
    int time_to_sleep_seconds = std::stoi(argv[2]);

    LOG_VICTIM << "Starting memory allocation stress test..." << std::endl;
    LOG_VICTIM << "Total memory to allocate: " << total_memory_to_allocate / (1024 * 1024) << std::endl;

    std::vector<void *> allocated_chunks;
    size_t allocated_memory = 0;

    while (allocated_memory < total_memory_to_allocate)
    {
        // Allocate a chunk of memory
        void *chunk = std::malloc(CHUNK_SIZE);
        if (!chunk)
        {
            ERROR_VICTIM << "Memory allocation failed at " << allocated_memory / (1024 * 1024) << " MB" << std::endl;
            break;
        }

        // Touch the memory to ensure it is actually allocated
        std::memset(chunk, 0, CHUNK_SIZE);

        // Keep track of the allocated chunk
        allocated_chunks.push_back(chunk);
        allocated_memory += CHUNK_SIZE;

        // std::cout << "Allocated " << allocated_memory / (1024 * 1024) << " MB" << std::endl;

        // Sleep for a short period to slow down the allocation process
        usleep(100000); // Sleep for 100 milliseconds
    }

    LOG_VICTIM << "Finished memory allocation stress test. Total allocated: " << allocated_memory / (1024 * 1024) << " MB" << std::endl;

    LOG_VICTIM << "Sleeping for " <<  time_to_sleep_seconds << std::endl;
    sleep(time_to_sleep_seconds);
    LOG_VICTIM << "Waking up from my sleep" << std::endl;

    // Free the allocated memory
    for (void *chunk : allocated_chunks)
    {
        std::free(chunk);
    }

    LOG_VICTIM << "freed all memory, exiting" << std::endl;
    return EXIT_SUCCESS;
}