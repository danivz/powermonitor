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

import sifive.blocks.util._

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
      new PMBusPort)
    with HasInterruptSources {

  def nInterrupts = 1

  lazy val module = new LazyModuleImp(this) {

  val cnt_max = params.clockFreqMHz * 5
  val freq_div_comm = Reg(init = UInt(0, log2Ceil(cnt_max).W))
  val freq_div_sampler = Reg(init = UInt(0, 32.W))
  val freq_div_error = Reg(init = UInt(0, log2Ceil(3*cnt_max).W))
  val memory = SeqMem(params.maxSamples, UInt(16.W))

  // CSR
  val prescaler = Reg(init = UInt(0x03FFFFFF, 32.W))
  val slave_addr = Reg(init = UInt(0, 8.W))
  val command = Reg(init = UInt(0, 8.W))
  val ctrl_status = Reg(init = UInt(0, 8.W))
  val data_addr = Reg(init = UInt(0, 16.W))
  val data = Reg(init = UInt(0, 16.W))
  val samples = Reg(init = UInt(0, 16.W))

  // Outputs
  val pmbus_clk = Reg(init = true.B)
  val pmbus_data = Reg(init = true.B)
  port.clk.oe := !pmbus_clk
  port.data.oe := !pmbus_data
  port.clk.out := false.B
  port.data.out := false.B

  // FSMs
  val s_main_idle :: s_main_acquire :: s_main_wait :: s_main_error :: Nil = Enum(UInt(), 4)
  val main_state = Reg(init = s_main_idle)

  val (s_comm_idle :: s_comm_start :: s_comm_addr_wr :: s_comm_addr_wr_ack :: s_comm_command ::
      s_comm_command_ack :: s_comm_reap_start :: s_comm_addr_rd :: s_comm_addr_rd_ack  ::
      s_comm_read_lo :: s_comm_read_lo_ack :: s_comm_read_hi :: s_comm_read_hi_ack ::
      s_comm_prestop :: s_comm_stop :: Nil) = Enum(UInt(), 15)
  val comm_state = Reg(init = s_comm_idle)

  val error = Reg(init = false.B)
  val data_buffer = Reg(init = UInt(0, 8.W))
  val data_aux = Reg(init = UInt(0, 16.W))
  val data_cnt = Reg(init = UInt(0, 3.W))
  val terminal_cnt_sampler = freq_div_sampler + 1.U === prescaler
  val terminal_cnt_comm = freq_div_comm + 1.U === cnt_max.U
  val terminal_cnt_error = freq_div_error + 1.U === (3*cnt_max).U

  // Main FSM
  switch(main_state) {
    is(s_main_idle) {
      when(ctrl_status(0)) {
        ctrl_status := 0.U
        samples := 0.U
        main_state := s_main_acquire
      }
    }
    is(s_main_acquire) {
      when(comm_state === s_comm_stop && terminal_cnt_comm) { 
        when(error) {
          ctrl_status := 0x06.U
          main_state := s_main_error
        }.elsewhen(ctrl_status(0)) {
          ctrl_status := 0x02.U
          main_state := s_main_idle
        }.otherwise { 
          memory.write(samples, data_aux)
          main_state := s_main_wait
        }
      }
    }
    is(s_main_wait) {
      when(samples + 1.U === params.maxSamples.U) {
        ctrl_status := 0x02.U
        main_state := s_main_idle 
      }.elsewhen(terminal_cnt_sampler) { 
        samples := samples + 1.U
        main_state := s_main_acquire
      }
    }
    is(s_main_error) {
      when(terminal_cnt_error) { main_state := s_main_acquire }
    }
  }

  when(main_state =/= s_main_idle) {
    when(terminal_cnt_sampler) { freq_div_sampler := 0.U }
    .otherwise { freq_div_sampler := freq_div_sampler + 1.U }
  }

  when(main_state === s_main_error) {
    when(terminal_cnt_error) {
      freq_div_error := 0.U
    }.otherwise {
      freq_div_error := freq_div_error + 1.U
    }
  }

  data := memory.read(data_addr)

  // PMBus FSM
  switch(comm_state) {
    is(s_comm_idle) {
      when(main_state === s_main_acquire) {
        pmbus_clk := true.B
        pmbus_data := false.B
        data_buffer := Cat(slave_addr, UInt(0, 1.W))
        error := false.B
        comm_state := s_comm_start
      }
    }
    is(s_comm_start) {
      when(!pmbus_clk) {
        pmbus_data := data_buffer(7)
        comm_state := s_comm_addr_wr
      }
    }
    is(s_comm_addr_wr) {
      when(terminal_cnt_comm && port.clk.in) {
        data_buffer := Cat(data_buffer(6,0), UInt(0, 1.W))
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) {
          data_buffer := command
          comm_state := s_comm_addr_wr_ack
          pmbus_data := true.B
        }.otherwise {
          pmbus_data := data_buffer(6)
        }
      }
    }
    is(s_comm_addr_wr_ack) {
      when(terminal_cnt_comm && port.clk.in) {
        when(!port.data.in) {
          pmbus_data := data_buffer(7)
          comm_state := s_comm_command
        }.otherwise { 
          comm_state := s_comm_prestop
          error := true.B
        }
      }
    }
    is(s_comm_command) {
      when(terminal_cnt_comm && port.clk.in) {
        data_buffer := Cat(data_buffer(6,0), UInt(0, 1.W))
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) {
          comm_state := s_comm_command_ack
          pmbus_data := true.B
        }.otherwise {
          pmbus_data := data_buffer(6)
        }
      }
    }
    is(s_comm_command_ack) {
      when(terminal_cnt_comm && port.clk.in) {
        when(!port.data.in) {
          comm_state := s_comm_reap_start
          data_buffer := Cat(slave_addr, UInt(1, 1.W))
        }.otherwise { 
          comm_state := s_comm_prestop
          error := true.B
        }
      }
    }
    is(s_comm_reap_start) {
      when(freq_div_comm + 1.U === (cnt_max/2).U && port.clk.in) {
        pmbus_data := false.B
      }.elsewhen(terminal_cnt_comm && port.clk.in) { comm_state := s_comm_addr_rd }
    }
    is(s_comm_addr_rd) {
      when(terminal_cnt_comm && port.clk.in) {
        data_buffer := Cat(data_buffer(6,0), UInt(0, 1.W))
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) {
          comm_state := s_comm_addr_rd_ack
          pmbus_data := true.B
        }.otherwise {
          pmbus_data := data_buffer(6)
        }
      }
    }
    is(s_comm_addr_rd_ack) {
      when(terminal_cnt_comm && port.clk.in) {
        when(!port.data.in) {
          comm_state := s_comm_read_lo
        }.otherwise { 
          comm_state := s_comm_prestop
          error := true.B
        }
      }
    }
    is(s_comm_read_lo) {
      when(terminal_cnt_comm && port.clk.in) {
        data_buffer := Cat(data_buffer(6,0), port.data.in)
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) {
          pmbus_data := false.B
          comm_state :=  s_comm_read_lo_ack
        }
      }
    }
    is(s_comm_read_lo_ack) {
      when(terminal_cnt_comm && port.clk.in) { 
        comm_state := s_comm_read_hi
        data_aux := data_buffer
        pmbus_data := true.B
      }
    }
    is(s_comm_read_hi) {
      when(terminal_cnt_comm && port.clk.in) {
        data_buffer := Cat(data_buffer(6,0), port.data.in)
        data_cnt := data_cnt + 1.U
        when(data_cnt + 1.U === 0.U) {
          comm_state :=  s_comm_read_hi_ack
        }
      }
    }
    is(s_comm_read_hi_ack) {
      when(terminal_cnt_comm && port.clk.in) { 
        comm_state := s_comm_prestop
        data_aux := Cat(data_buffer, data_aux(7,0))
      }
    }
    is(s_comm_prestop) {
      pmbus_clk := false.B
      pmbus_data := false.B
      when(terminal_cnt_comm) { comm_state := s_comm_stop }
    }
    is(s_comm_stop) {
        pmbus_clk := true.B
      when(terminal_cnt_comm) {
        comm_state := s_comm_idle
        pmbus_data := true.B
      }
    }
  }

  // PMBus frequency generator
  val clk_en = comm_state === s_comm_start || comm_state === s_comm_addr_wr || comm_state === s_comm_addr_wr_ack || 
               comm_state === s_comm_command || comm_state === s_comm_command_ack || comm_state === s_comm_reap_start || 
               comm_state === s_comm_addr_rd || comm_state === s_comm_addr_rd_ack || comm_state === s_comm_read_lo || 
               comm_state === s_comm_read_lo_ack || comm_state === s_comm_read_hi || comm_state === s_comm_read_hi_ack

  when(comm_state =/= s_comm_idle) {
    when((pmbus_clk && port.clk.in) || !pmbus_clk) { freq_div_comm := freq_div_comm + 1.U }
    when(terminal_cnt_comm) {
      freq_div_comm := 0.U
      when(clk_en) { pmbus_clk := !pmbus_clk }
    }
  }

  // CSR mapping
  regmap(
    PowerMonitorCtrlRegs.prescaler -> Seq(RegField(32, prescaler, 
        RegFieldDesc("prescaler", "PowerMonitor sample frequency prescaler", reset=Some(0x0000FFFF)))),
    PowerMonitorCtrlRegs.slave_addr -> Seq(RegField(8, slave_addr, 
        RegFieldDesc("slave_addr", "PowerMonitor slave address register", reset=Some(0)))),
    PowerMonitorCtrlRegs.command -> Seq(RegField(8, command, 
        RegFieldDesc("command", "PowerMonitor PMBus command register", reset=Some(0)))),
    PowerMonitorCtrlRegs.ctrl_status -> Seq(RegField(8, ctrl_status,
        RegFieldDesc("ctrl_status", "PowerMonitor control and status register", reset=Some(0)))),
    PowerMonitorCtrlRegs.data_addr -> Seq(RegField(16, data_addr, 
        RegFieldDesc("data_addr", "PowerMonitor memory address", reset=Some(0)))),
    PowerMonitorCtrlRegs.data -> Seq(RegField(16, data, 
        RegFieldDesc("data", "PowerMonitor data value readed with PMBus", reset=Some(0)))),
    PowerMonitorCtrlRegs.samples -> Seq(RegField(16, samples, 
        RegFieldDesc("samples", "PowerMonitor number of samples to read", reset=Some(0))))
  )
  
  interrupts(0) := false.B
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
