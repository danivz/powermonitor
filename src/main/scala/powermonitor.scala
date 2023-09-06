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
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.{AsyncResetRegVec, Majority}


case class PowerMonitorParams(
  address: BigInt,
  clockFreqMHz: Int = 50,
  maxSamples: Int = 2048) extends DeviceParams

class PMBusPin extends Bundle {
  val in  = Bool(INPUT)
  val out = Bool(OUTPUT)
  val oe  = Bool(OUTPUT)
}

class PMBusPort extends Bundle {
  val clk = new PMBusPin
  val data = new PMBusPin
}

abstract class PowerMonitor(busWidthBytes: Int, params: PowerMonitorParams)(implicit p: Parameters)
    extends IORegisterRouter(
      RegisterRouterParams(
        name = "powermonitor",
        compat = Seq("ceiupm,pwrmon"),
        base = params.address,
        beatBytes = busWidthBytes),
      new PMBusPort) {

  lazy val module = new LazyModuleImp(this) {

  val cnt_max = params.clockFreqMHz * 5
  val freq_div = Reg(init = UInt(0, log2Ceil(cnt_max).W))
  val cmd_read_pout = UInt(0x96, 8.W)

  class ControlStatusBundle extends Bundle{
    val start       = Bool()
    val done        = Bool()
    val read_enable = Bool()
    val reserved    = UInt(5.W)
    val slave_addr  = UInt(8.W)
  }

  // CSR
  val prescaler = Reg(init = UInt(0x0000FFFF, 32.W))
  val ctrl_status = Reg(init = (new ControlStatusBundle).fromBits(0.U)))
  val data_addr = Reg(init = UInt(0, 32.W))
  val data = Reg(init = UInt(0, 32.W))

  // Outputs
  val pmbus_clk = Reg(init = false.B)
  val pmbus_data = Reg(init = false.B)
  port.clk.oe := !pmbus_clk
  port.data.oe := !pmbus_data
  port.clk.out := false.B
  port.data.out := false.B

  // Main FSM
  val s_main_idle :: s_main_acquire :: s_main_wait :: s_main_done :: Nil = Enum(UInt(), 4)
  val main_state = Reg(init = s_main_idle)

  switch(main_state) {
    is(s_main_idle) {
      when(ctrl_status.start) {
        ctrl_status.start := false.B
        main_state := s_main_acquire
      }
    }
    is(s_main_acquire) {
      main_state := s_main_wait
    }
    is(s_main_wait) {
      main_state := s_main_done
    }
    is(s_main_done) {
      ctrl_status.done := true.B
      main_state := s_main_idle
    }
  }

  // PMBus FSM
  val s_comm_idle :: s_comm_start :: s_comm_addr :: s_comm_addr_ack :: s_comm_data :: 
      s_comm_data_ack :: s_comm_wait :: s_comm_read :: s_comm_read_ack :: s_comm_stop :: Nil = Enum(UInt(), 10)
  val comm_state = Reg(init = s_comm_idle)
  val data_buffer = Reg(init = UInt(0, 8.W))
  val data_cnt = Reg(init = UInt(0, 3.W))

  switch(comm_state) {
    is(s_comm_idle) {
      when(main_state === s_main_acquire) {
        pmbus_clk := true.B
        pmbus_data := false.B
        data_buffer := ctrl_status.slave_addr
        comm_state := s_comm_start
      }
    }
    is(s_comm_start) {
      when(!pmbus_clk) {
        pmbus_data := data_buffer(0)
        comm_state := s_comm_addr
      }
    }
    is(s_comm_addr) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        data_buffer := Cat(UInt(0, 1.W), data_buffer(7,1))
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 7.U) {
          data_buffer := cmd_read_pout
          comm_state := s_comm_addr_ack
          pmbus_data := true.B
          data_cnt := 0.U
        }.otherwise {
          pmbus_data := data_buffer(1)
        }
      }
    }
    is(s_comm_addr_ack) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        when(!port.data.in) {
          pmbus_data := data_buffer(0)
          comm_state := s_comm_data
        }.otherwise { comm_state := s_comm_stop }
      }
    }
    is(s_comm_data) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        data_buffer := Cat(UInt(0, 1.W), data_buffer(7,1))
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) {
          comm_state := s_comm_data_ack
          pmbus_data := true.B
        }.otherwise {
          pmbus_data := data_buffer(1)
        }
      }
    }
    is(s_comm_data_ack) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        when(!port.data.in) {
          pmbus_data := true.B
          comm_state := s_comm_wait
        }.otherwise { comm_state := s_comm_stop }
      }
    }
    is(s_comm_wait) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) { comm_state :=  s_comm_read}
      }
    }
    is(s_comm_read) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        data_buffer := Cat(data_buffer(6,0), port.data.in)
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) { 
          pmbus_data := false.B
          comm_state :=  s_comm_read_ack
        }
      }
    }
    is(s_comm_read_ack) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        pmbus_clk := true.B 
        comm_state := s_comm_stop
      }
    }
    is(s_comm_stop) {
      when(freq_div + 1.U === 0.U && pmbus_clk) {
        pmbus_data := true.B
        comm_state := s_comm_idle
      }
    }
  }

  // PMBus frequency generator
  val clk_en = comm_state === s_comm_start || comm_state === s_comm_addr || comm_state === s_comm_addr_ack || 
               comm_state === s_comm_data || comm_state === s_comm_data_ack

  when(clk_en) { 
    freq_div := freq_div + 1.U
    when(freq_div + 1.U === 0.U) pmbus_clk := !pmbus_clk
  }

  // CSR mapping
  regmap(
    PowerMonitorCtrlRegs.prescaler -> Seq(RegField(32, prescaler, 
        RegFieldDesc("prescaler", "PowerMonitor sample frequency prescaler", reset=Some(0)))),
    PowerMonitorCtrlRegs.ctrl_status -> RegFieldGroup("control", Some("Control"),
        ctrl_status.elements.map{
            case(name, e) => RegField(e.getWidth,
                                      e.asInstanceOf[UInt],
                    RegFieldDesc(name, s"Sets the ${name}" ,
                    reset=Some(0)))
        }.toSeq),
    PowerMonitorCtrlRegs.data_addr -> Seq(RegField(32, data_addr, 
        RegFieldDesc("data_addr", "PowerMonitor memory address", reset=Some(0)))),
    PowerMonitorCtrlRegs.data -> Seq(RegField(32, data, 
        RegFieldDesc("data", "PowerMonitor data value readed with PMBus", reset=Some(0))))
  )

  ctrl_status.reserved := 0.U
}}

class TLPowerMonitor(busWidthBytes: Int, params: PowerMonitorParams)(implicit p: Parameters)
  extends PowerMonitor(busWidthBytes, params) with HasTLControlRegMap

case class PowerMonitorLocated(loc: HierarchicalLocation) extends Field[Seq[PowerMonitorAttachParams]](Nil)

case class PowerMonitorAttachParams(
  device: PowerMonitorParams,
  controlWhere: TLBusWrapperLocation = PBUS,
  blockerAddr: Option[BigInt] = None,
  controlXType: ClockCrossingType = NoCrossing,
  intXType: ClockCrossingType = NoCrossing) extends DeviceAttachParams
{
  def attachTo(where: Attachable)(implicit p: Parameters): TLPowerMonitor = where {
    val name = s"powermonitor_${PowerMonitor.nextId()}"
    val tlbus = where.locateTLBusWrapper(controlWhere)
    val powermonitorClockDomainWrapper = LazyModule(new ClockSinkDomain(take = None))
    val powermonitor = powermonitorClockDomainWrapper { LazyModule(new TLPowerMonitor(tlbus.beatBytes, device)) }
    powermonitor.suggestName(name)

    tlbus.coupleTo(s"device_named_$name") { bus =>

      val blockerOpt = blockerAddr.map { a =>
        val blocker = LazyModule(new TLClockBlocker(BasicBusBlockerParams(a, tlbus.beatBytes, tlbus.beatBytes)))
        tlbus.coupleTo(s"bus_blocker_for_$name") { blocker.controlNode := TLFragmenter(tlbus) := _ }
        blocker
      }

      powermonitorClockDomainWrapper.clockNode := (controlXType match {
        case _: SynchronousCrossing =>
          tlbus.dtsClk.map(_.bind(powermonitor.device))
          tlbus.fixedClockNode
        case _: RationalCrossing =>
          tlbus.clockNode
        case _: AsynchronousCrossing =>
          val powermonitorClockGroup = ClockGroup()
          powermonitorClockGroup := where.asyncClockGroupsNode
          blockerOpt.map { _.clockNode := powermonitorClockGroup } .getOrElse { powermonitorClockGroup }
      })

      (powermonitor.controlXing(controlXType)
        := TLFragmenter(tlbus)
        := blockerOpt.map { _.node := bus } .getOrElse { bus })
    }

    (intXType match {
      case _: SynchronousCrossing => where.ibus.fromSync
      case _: RationalCrossing => where.ibus.fromRational
      case _: AsynchronousCrossing => where.ibus.fromAsync
    }) := powermonitor.intXing(intXType)

    powermonitor
  }
}

object PowerMonitor {
  val nextId = { var i = -1; () => { i += 1; i} }

  def makePort(node: BundleBridgeSource[PMBusPort], name: String)(implicit p: Parameters): ModuleValue[PMBusPort] = {
    val powermonitorNode = node.makeSink()
    InModuleBody { powermonitorNode.makeIO()(ValName(name)) }
  }
}