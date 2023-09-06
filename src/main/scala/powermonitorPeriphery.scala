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
import org.chipsalliance.cde.config.Field
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.{BaseSubsystem}


case object PeripheryPowerMonitorKey extends Field[Seq[PowerMonitorParams]](Nil)

trait HasPeripheryPowerMonitor { this: BaseSubsystem =>
  val powermonitorNodes =  p(PeripheryPowerMonitorKey).map { ps =>
    PowerMonitorAttachParams(ps).attachTo(this).ioNode.makeSink()
  }
}

trait HasPeripheryPowerMonitorBundle {
  val powermonitor: Seq[PMBusPort]
}

trait HasPeripheryPowerMonitorModuleImp extends LazyModuleImp with HasPeripheryPowerMonitorBundle {
  val outer: HasPeripheryPowerMonitor
  val powermonitor  = outer.powermonitorNodes.zipWithIndex.map  { case(n,i) => n.makeIO()(ValName(s"powermonitor_$i")) }
}
