# PowerMonitor - FPGA Power Consumption Module for Xilinx FPGAs

PowerMonitor is a hardware accelerator designed to measure power consumption in Xilinx FPGAs using the PMBus protocol (tested on the VC709 Xilinx evaluation board). This accelerator is specifically developed for integration as a memory-mapped module using the TileLink protocol in Chipyard SoC (version 1.10.0).

## Description

PowerMonitor is an essential tool for measuring and monitoring real-time power consumption in Xilinx FPGAs. It provides accurate and detailed energy consumption data, enabling developers to optimize the performance and efficiency of their FPGA designs.

PowerMonitor is integrated into the Chipyard SoC using the TileLink protocol, making it easy to access and control from application software in the system. This allows developers to efficiently access power consumption data and make adjustments based on the results.

## Key Features

- Precise measurement of power consumption in Xilinx FPGAs.
- Integration using the TileLink protocol in Chipyard 1.10.0.
- Simple control interface for configuration and data retrieval.
- Support for the PMBus protocol for communication with PMBus devices present on Xilinx FPGAs.
- Real-time data and energy consumption history storage.

## Requirements

To use PowerMonitor, you will need the following:

- A Chipyard-based SoC system running version 1.10.0.
- Xilinx FPGAs compatible with the PMBus protocol.
- Access to the TileLink infrastructure in your SoC for integration.

## Integration into Chipyard

To integrate PowerMonitor into your Chipyard SoC, follow these steps:

1. Clone this repository into the appropriate directory of your Chipyard project.
2. Add the PowerMonitor module to your Chipyard SoC description.

## Contributing

If you would like to contribute to this project, please fork the repository and create a pull request with your changes. We welcome contributions that improve the functionality or usability of the code, or that provide additional features.

## License

This project is licensed under the GNU General Public License v3.0. Please see the [LICENSE](./LICENSE.md) file for more information.

## Contact

If you have any questions or concerns about this project, please contact the project owner at [mail](mailto:daniel.vazquez@upm.es). See the [AUTHORS](./AUTHORS.md) file for more information.
