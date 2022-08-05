/*
 * %file%
 *
 * Create Date : %date%
 * Copyright (c) 2019- %name% <%mail%>
 */
#include <filesystem>
#include <iostream>

#include <glog/logging.h>
#include <gperftools/profiler.h>

namespace fs = std::filesystem;

int main(int argc, char** argv) {
    // logging codes
    google::InitGoogleLogging(argv[0]);
    google::InstallFailureSignalHandler();

    fs::path save_dir("./prof");
    if (!fs::exists(save_dir)) {
        fs::create_directories(save_dir);
    }
    auto save_path =
        (save_dir / fs::path((fs::path(argv[0])).stem().string() + ".prof")).generic_string();
    ProfilerStart(save_path.c_str());
    std::cout << "Profiling to " << save_path << std::endl;
    // end logging codes

    // Insert your codes here!!!




    // finish program
    ProfilerStop();
}
