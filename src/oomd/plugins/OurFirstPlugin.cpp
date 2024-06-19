#include "oomd/plugins/OurFirstPlugin.h"

#include "oomd/Log.h"
#include "oomd/PluginRegistry.h"
#include "oomd/util/Util.h"

#include <iostream>
#include <fstream> 

namespace Oomd {

REGISTER_PLUGIN(our_first_plugin, OurFirstPlugin::create);

int OurFirstPlugin::init(
    const Engine::PluginArgs& args,
    const PluginConstructionContext& context) {

  argParser_.addArgument("message", message_, true);

  if (!argParser_.parse(args)) {
    return 1;
  }

  // Success
  return 0;
}

Engine::PluginRet OurFirstPlugin::run(OomdContext& ctx) {
    std::ofstream outFile("/example.txt", std::ios::app);

    // Check if the file was opened successfully
    if (outFile.is_open()) {
        // Write a simple message to the file
        outFile << "Nitzan and Guy are kings and the message is:\n" 
                << message_
                << "\n";

        // Close the file stream
        outFile.close(); 
    }
}

} // namespace Oomd