#include "Util.h"
#include <fstream>
#include <stdexcept>
#include <cstring>
#include "oomd/Log.h"

void writeToFile(const std::string& path, const std::string& value) {
    std::ofstream file(path);
    if (!file.is_open()) {
        OLOG << "Error opening file: " << path << " - " << strerror(errno);
        throw std::runtime_error("Failed to open file: " + path);
    }
    file << value;
    if (file.fail()) {
        OLOG << "Error writing to file: " << path << " - " << strerror(errno);
        throw std::runtime_error("Failed to write to file: " + path);
    }
    file.close();
    OLOG << "Successfully wrote to file: " << path;
}