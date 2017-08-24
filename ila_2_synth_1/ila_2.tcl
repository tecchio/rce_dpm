# 
# Synthesis run script generated by Vivado
# 

set_param gui.test TreeTableDev
set_param xicom.use_bs_reader 1
debug::add_scope template.lib 1
set_msg_config -id {HDL 9-1061} -limit 100000
set_msg_config -id {HDL 9-1654} -limit 100000
set_msg_config  -id {VRFC 10-165}  -suppress 
set_msg_config  -id {HDL 9-1061}  -suppress 
set_msg_config  -id {VRFC 10-163}  -suppress 
set_msg_config  -id {Vivado 12-1411}  -new_severity {ERROR} 
set_msg_config  -id {Route 35-14}  -new_severity {ERROR} 
set_msg_config  -id {HDL 9-806}  -new_severity {ERROR} 
set_msg_config  -id {Timing 38-3}  -new_severity {INFO} 
set_msg_config  -id {Opt 31-80}  -new_severity {ERROR} 
set_msg_config  -id {Synth 8-153}  -new_severity {ERROR} 
set_msg_config  -id {Common 17-301}  -suppress 
set_msg_config  -id {Synth 8-3512}  -new_severity {ERROR} 
set_msg_config  -id {Synth 8-312}  -suppress 
set_msg_config  -id {Synth 8-638}  -suppress 
set_msg_config  -id {Synth 8-4472}  -suppress 
set_msg_config  -id {Runs 36-5}  -suppress 
set_msg_config  -id {Synth 8-637}  -suppress 
set_msg_config  -id {Synth 8-3330}  -new_severity {CRITICAL WARNING} 
set_msg_config  -id {Vivado 12-4430}  -new_severity {WARNING} 
set_msg_config  -id {Simtcl 6-16}  -suppress 
set_msg_config  -id {Designutils 20-1318}  -suppress 
set_msg_config  -id {Simtcl 6-17}  -suppress 
set_msg_config  -id {Pwropt 34-142}  -suppress 
set_msg_config  -id {Drc 23-20}  -suppress 
set_msg_config  -id {Synth 8-226}  -suppress 
set_msg_config  -id {Synth 8-3352}  -new_severity {ERROR} 
set_msg_config  -id {Synth 8-113}  -suppress 
set_msg_config  -id {Synth 8-63}  -new_severity {ERROR} 
set_msg_config  -id {Synth 8-3919}  -new_severity {CRITICAL WARNING} 
set_msg_config  -id {VRFC 10-664}  -new_severity {ERROR} 
set_msg_config  -id {Synth 8-256}  -suppress 
set_msg_config  -id {Vivado 12-1387}  -new_severity {WARNING} 
set_msg_config  -id {Vivado 12-508}  -new_severity {CRITICAL WARNING} 

create_project -in_memory -part xc7z045ffg900-2
set_param project.compositeFile.enableAutoGeneration 0
set_param synth.vivado.isSynthRun true
set_msg_config -id {IP_Flow 19-2162} -severity warning -new_severity info
set_property webtalk.parent_dir C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.cache/wt [current_project]
set_property parent.project_path C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.xpr [current_project]
set_property default_lib xil_defaultlib [current_project]
set_property target_language VHDL [current_project]
read_ip C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2.xci
set_property used_in_implementation false [get_files -all c:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2.dcp]
set_property is_locked true [get_files C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2.xci]

catch { write_hwdef -file ila_2.hwdef }
synth_design -top ila_2 -part xc7z045ffg900-2 -mode out_of_context
rename_ref -prefix_all ila_2_
write_checkpoint -noxdef ila_2.dcp
catch { report_utilization -file ila_2_utilization_synth.rpt -pb ila_2_utilization_synth.pb }
if { [catch {
  file copy -force C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.runs/ila_2_synth_1/ila_2.dcp C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2.dcp
} _RESULT ] } { 
  error "ERROR: Unable to successfully create or copy the sub-design checkpoint file."
}
if { [catch {
  write_verilog -force -mode synth_stub C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2_stub.v
} _RESULT ] } { 
  puts "CRITICAL WARNING: Unable to successfully create a Verilog synthesis stub for the sub-design. This may lead to errors in top level synthesis of the design. Error reported: $_RESULT"
}
if { [catch {
  write_vhdl -force -mode synth_stub C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2_stub.vhdl
} _RESULT ] } { 
  puts "CRITICAL WARNING: Unable to successfully create a VHDL synthesis stub for the sub-design. This may lead to errors in top level synthesis of the design. Error reported: $_RESULT"
}
if { [catch {
  write_verilog -force -mode funcsim C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2_funcsim.v
} _RESULT ] } { 
  puts "CRITICAL WARNING: Unable to successfully create the Verilog functional simulation sub-design file. Post-Synthesis Functional Simulation with this file may not be possible or may give incorrect results. Error reported: $_RESULT"
}
if { [catch {
  write_vhdl -force -mode funcsim C:/Users/tecchio/Desktop/RCE/10GbAxi_kotodpm_usedma_probe/10GbAxi_kotodpm_usedma_probe.srcs/sources_1/ip/ila_2/ila_2_funcsim.vhdl
} _RESULT ] } { 
  puts "CRITICAL WARNING: Unable to successfully create the VHDL functional simulation sub-design file. Post-Synthesis Functional Simulation with this file may not be possible or may give incorrect results. Error reported: $_RESULT"
}
