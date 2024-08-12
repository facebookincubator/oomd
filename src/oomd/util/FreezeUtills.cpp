#include "./FreezeUtills.h"
#include "../Log.h"

#include <fstream>
#include <stdexcept>

namespace Oomd {

void logError(const std::string& message) {
  std::system_error error(errno, std::generic_category(), message);
  OLOG << "Error: " << error.what();
}

void writeToFile(
    const std::string& path,
    const std::string& value) {
  std::ofstream file(path);
  if (!file.is_open()) {
    logError("open file: " + path);
  }
  file << value;
  if (file.fail()) {
    logError("Error writing to file: " + path);
    file.close();
    return;
  }
  file.close();
  OLOG << "Successfully wrote to file: " + path;
}

} // namespace Oomd