#include <iostream>
#include <sys/mman.h>
#include <unistd.h>

int main() {
    // Allocate a memory page using mmap for correct alignment
    void* addr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (addr == MAP_FAILED) {
        std::perror("mmap");
        return 1;
    }

    // Call madvise with MADV_FREE
    if (madvise(addr, 4096, MADV_FREE) != 0) {
        std::perror("madvise");
        munmap(addr, 4096);
        return 1;
    }

    // Free the allocated memory using munmap
    if (munmap(addr, 4096) != 0) {
        std::perror("munmap");
        return 1;
    }

    return 0;
}