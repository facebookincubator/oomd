// simple_process.cpp
#include <iostream>
#include <unistd.h>

int main() {
    std::cout << "Process running with PID: " << getpid() << std::endl;
    sleep(600);
    
    return 0;
}