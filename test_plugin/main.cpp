#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../../../usr/include/x86_64-linux-gnu/sys/stat.h"
#include <cerrno>
#include <iostream>
#include <sys/mman.h>
#include <sys/syscall.h>


// Define SYS_process_madvise and SYS_pidfd_open if not defined
#ifndef SYS_process_madvise
#define SYS_process_madvise 443  // Update this if necessary for your architecture
#endif

#ifndef SYS_pidfd_open
#define SYS_pidfd_open 434  // Update this if necessary for your architecture
#endif

#define CGROUP_PATH "/sys/fs/cgroup/freezer/my_freezer"


// Wrapper for pidfd_open syscall
int pidfd_open(pid_t pid, unsigned int flags) {
    return syscall(SYS_pidfd_open, pid, flags);
}

// Wrapper function for process_madvise
long process_madvise(pid_t pid, const struct iovec *vec, size_t vlen, int advice, unsigned long flags) {
    int pidfd = pidfd_open(pid, 0);
     if (pidfd == -1) {
        perror("pidfd_open");
        return EXIT_FAILURE;
    }

    return syscall(SYS_process_madvise, pidfd, vec, vlen, advice, flags);
}

void write_to_file(const char *path, const char *value) {
    int fd = open(path, O_WRONLY);
    if (fd == -1) {
        perror("open");
        exit(EXIT_FAILURE);
    }
    if (write(fd, value, strlen(value)) == -1) {
        perror("write");
        close(fd);
        exit(EXIT_FAILURE);
    }
    close(fd);
}
 
bool create_freeze_cgroup() {
    // Create the cgroup directory
    if (mkdir(CGROUP_PATH, 0755) && errno != EEXIST) {
        perror("mkdir");
        return false;
    }
    return true;
}

bool page_out_memory(int pid) {
    // Open and read /proc/[pid]/maps
    char maps_path[256];
    snprintf(maps_path, sizeof(maps_path), "/proc/%d/maps", pid);
    FILE *maps_file = fopen(maps_path, "r");
    if (!maps_file) {
        perror("fopen");
        return false;
    }
 
    // Parse memory regions
    char line[256];
    unsigned long start, end;
    while (fgets(line, sizeof(line), maps_file)) {
        if (sscanf(line, "%lx-%lx", &start, &end) == 2) {
            struct iovec iov = {
                .iov_base = (void *)start,
                .iov_len = end - start,
            };
            if (process_madvise(pid, &iov, 1, MADV_PAGEOUT, 0) == -1) {
                perror("process_madvise");
            }
            else {
                std::cout << "Memory at " << iov.iov_base << " was paged out\n";
            }
        }
    }
    fclose(maps_file);
    return true;
}

void freeze_process(int pid) {

    // page out memory
    if(!page_out_memory(pid)) {
        return;
    }
    std::cout << "Memory paged out!";

    // Add the process to the cgroup
    char tasks_path[256];
    snprintf(tasks_path, sizeof(tasks_path), "%s/tasks", CGROUP_PATH);
    char pid_str[16];
    snprintf(pid_str, sizeof(pid_str), "%d", pid);
    write_to_file(tasks_path, pid_str);

    // Freeze the process
    char state_path[256];
    snprintf(state_path, sizeof(state_path), "%s/freezer.state", CGROUP_PATH);
    write_to_file(state_path, "FROZEN");
    std::cout << "Process: " << pid << " is frozen!\n";
}


// void unfreeze(const char* state_path, const char* tasks_path, const char* pid_str) {
//     // Unfreeze the process
//     write_to_file(state_path, "THAWED");

//     // Remove the process from the cgroup
//     write_to_file(tasks_path, pid_str);
// }

void handle_process(int pid) {
    std::cout << "Freezing process: " << pid << std::endl;
    
    if(!create_freeze_cgroup()) {
        return;
    }
    freeze_process(pid);
 
    // // Remove the cgroup directory
    // if (rmdir(CGROUP_PATH) == -1) {
    //     perror("rmdir");
    // }
}
 

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <pid>\n", argv[0]);
        return EXIT_FAILURE;
    }
    int pid = atoi(argv[1]);
    handle_process(pid);
    return EXIT_SUCCESS;
}

// void page_out_memory(int pid) {
//     // Open and read /proc/[pid]/maps
//     char maps_path[256];
//     snprintf(maps_path, sizeof(maps_path), "/proc/%d/maps", pid);
//     FILE *maps_file = fopen(maps_path, "r");
//     if (!maps_file) {
//         perror("fopen");
//         write_to_file(state_path, "THAWED");
//         return;
//     }
 
//     // Parse memory regions
//     char line[256];
//     unsigned long start, end;
//     while (fgets(line, sizeof(line), maps_file)) {
//         if (sscanf(line, "%lx-%lx", &start, &end) == 2) {
//             struct iovec iov = {
//                 .iov_base = (void *)start,
//                 .iov_len = end - start,
//             };
//             if (process_madvise(pid, &iov, 1, MADV_PAGEOUT, 0) == -1) {
//                 perror("process_madvise");
//             }
//         }
//     }
//     fclose(maps_file);
// }