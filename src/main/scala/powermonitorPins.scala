////////////////////////////////////////////////////////////////////
//                                                                //
//   d8888   o888                                                 //
//  d888888  O888                                ,--.""           //
//       88   88  V888888                __,----( o ))            //
//       88  88      88P               ,'--.      , (             //
//       88 88     88P          -"",:-(    o ),-'/  ;             //
//       8888    d8P              ( o) `o  _,'\ / ;(              //
//       888    888888888P         `-;_-<'\_|-'/ '  )             //
//                                     `.`-.__/ '   |             //
//                        \`.            `. .__,   ;              //
//                         )_;--.         \`       |              //
//                        /'(__,-:         )      ;               //
//                      ;'    (_,-:     _,::     .|               //
//                     ;       ( , ) _,':::'    ,;                //
//                    ;         )-,;'  `:'     .::                //
//                    |         `'  ;         `:::\               //
//                    :       ,'    '            `:\              //
//                    ;:    '  _,-':         .'     `-.           //
//                     ';::..,'  ' ,        `   ,__    `.         //
//                       `;''   / ;           _;_,-'     `.       //
//                             /            _;--.          \      //
//                           ,'            / ,'  `.         \     //
//                          /:            (_(   ,' \         )    //
//                         /:.               \_(  /-. .:::,;/     //
//                        (::..                 `-'\ "`""'        //
////////////////////////////////////////////////////////////////////
//                                                                //
//  Daniel Vazquez,  daniel.vazquez@upm.es                        //
//                                                                //
//  Centro de Electronica Industrial (CEI)                        //
//  Universidad Politecnica de Madrid (UPM)                       //
//                                                                //
////////////////////////////////////////////////////////////////////

package powermonitor

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import chisel3.{withClockAndReset}
import freechips.rocketchip.util.SyncResetSynchronizerShiftReg
import sifive.blocks.devices.pinctrl.{Pin, PinCtrl}


class PMBusSignals[T <: Data](private val pingen: () => T) extends Bundle {
  val clk: T = pingen()
  val data: T = pingen()
}

class PMBusPins[T <: Pin](pingen: () => T) extends PMBusSignals[T](pingen)

object PMBusPinsFromPort {

  def apply[T <: Pin](pins: PMBusSignals[T], pmbus: PMBusPort, clock: Clock, reset: Bool, syncStages: Int = 0) = {
    withClockAndReset(clock, reset) {
      pins.clk.outputPin(pmbus.clk.out, pue=true.B, ie = true.B)
      pins.clk.o.oe := pmbus.clk.oe
      pmbus.clk.in := SyncResetSynchronizerShiftReg(pins.clk.i.ival, syncStages, init = Bool(true),
        name = Some("pmbus_clk_sync"))

      pins.data.outputPin(pmbus.data.out, pue=true.B, ie = true.B)
      pins.data.o.oe := pmbus.data.oe
      pmbus.data.in := SyncResetSynchronizerShiftReg(pins.data.i.ival, syncStages, init = Bool(true),
        name = Some("pmbus_data_sync"))
    }
  }
}
