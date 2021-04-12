
Design Space Explorer
=====================

<!--- This file was modified by contributors from Intel Labs -->

Description
-----------

This automation provides two scripts - 1) dseplorery.py and 2) create_json.py

1) dsexplorer.py: This script creates VTA configurations that are architecturally valid in the 5-dimensional space (batch, block, inp_buff, wgt_buff, acc_buff). 

The configurations are represented in the format mentioned in the tophub file, eg. 1x16_15_15_18_17_i8w8a32 represents the default VTA configuration. In this phase, the data widths are not being varied, so the output of this script has the configuration represented as 1x16_15_15_18_17

Usage directions are provided below. 
Ensure that $TVM_HOME is set to be the TVM directory.

2) create_json.py: This script creates the json file for running the pytest-based tests on custom VTA configurations

Usage
-----

1) dsexplorer.py: By default, this script proposes configurations with 256 MAC units (that can be changed by command-line argument). Only specify non-default VTA parameters in the command-line and all compatible configurations will be specified

$ python dsexplorer.py --help
List all the available arguments of the script

Example

$ python dsexplorer.py -ba 2 -bl 3 -ib 18 : Creates VTA configurations with LOG_BATCH = 2, LOG_BLOCK = 3, LOG_INP_BUFF_SIZE=18 and 256 MAC units

Output:
4x8_15_18_15_15
4x8_15_18_15_16
4x8_15_18_15_17
4x8_15_18_16_15
4x8_15_18_16_16
4x8_15_18_17_15
6 configurations found.

Presently, the script searches for configurations in a space of maximum LOG_BATCH=4, LOG_BLOCK=6, maximum and minimum buffer sizes of 18 and 15 (log values). These all can be changed as command-line arguments.

2) create_json.py: This program creates the json file depending on the config and the target passed on as command-line parameters. It assumes that vta_config.json has the default VTA parameters

$ python create_json.py --help
List all the available arguments of the script

Example

$ python create_json.py -c 4x8_15_18_15_15 -t tsim : Creates $TVM_HOME/3rdparty/vta-hw/config/tsim_4x8_15_18_15_15_sample.json with VTA config parameters defined by the -c flag.

This format of json file naming is compatible with the format being used in the verification package now.

Example

$TVM_HOME$ VTA_TARGET=tsim_4x8_15_18_15_15 make ci-build
$TVM_HOME/verif$ pytest --co --targets tsim_4x8_15_18_15_15
