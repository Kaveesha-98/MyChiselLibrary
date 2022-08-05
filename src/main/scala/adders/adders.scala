package adders

import chisel3._
import chisel3.util._
import chisel3.Driver

sealed trait add
case class cla_add(width: Int, withOverFlow: Boolean) extends add
case class ripple_add(width: Int, withOverFlow: Boolean) extends add
case class pipelined_add_with_cla(stages: Int, width: Int, withOverFlow: Boolean) extends add
//case class pipelined_adder_with_ripple(stages: Int, width: Int, withOverFlow: Boolean)

abstract class Adder(width: Int, withOverFlow: Boolean) extends Module {
	val io = IO(new Bundle{
		val A = Input(UInt(width.W))
		val B = Input(UInt(width.W))
		val Cin = Input(UInt(1.W))
		val sum = Output(UInt((if (withOverFlow) (width+1) else width).W))
	})
}

object Adder {

	private abstract class singleCycleAdder(val width: Int, val withOverFlow: Boolean) extends Adder(width, withOverFlow) {
	
		val P = io.A ^ io.B
		val G = io.A & io.B
		
		def C: UInt
		
		io.sum := C ^ P
		
	}
	
	private trait cla extends singleCycleAdder{
	
		def getPartialProduct(carry_index: Int)(product_index: Int) = 
			if (carry_index == 0) io.Cin
			else if (product_index == 0) (P(carry_index - 1, 0).andR & io.Cin)
			else if (product_index == carry_index) G(carry_index - 1)
			else (P(carry_index - 1, product_index).andR & G(product_index - 1))
				
		def partialProducts(carry_index: Int) = 
			Seq.tabulate(carry_index + 1)(getPartialProduct(carry_index)_)

		override def C = Cat(Seq.tabulate(width+1)(i => partialProducts(i).reduce(_ | _)).reverse)
	
	}
	
	private trait ripple extends singleCycleAdder{
		
		override def C = Cat((Seq.tabulate(width)(P(_))).zip(Seq.tabulate(width)(G(_))).
				scanLeft(io.Cin)((carryIn: UInt, PG: (UInt, UInt)) => PG._2 | (PG._1 & carryIn)).reverse)
	
	}
	
	private class pipelined_adder(stages: Int, width: Int, withOverFlow: Boolean) extends Adder(width, withOverFlow) {
	
		val stageWidth = width/stages									//adder width for a single pipeline stage
		val singleStageAdd = generateAdder(cla_add(stageWidth, withOverFlow))_	//adder for the pipeline width
		
		val A = Seq.tabulate(stages-1)(i => Wire(UInt((width - stageWidth*(i+1)).W))).	//creating wires for adder input A
				scan(RegNext(io.A))((prev: UInt, next: UInt) => {
					next := RegNext(prev >> stageWidth)									//creating a delay for the adder inputs
					next
				})
				
		val B = Seq.tabulate(stages-1)(i => Wire(UInt((width - stageWidth*(i+1)).W))).
				scan(RegNext(io.B))((prev: UInt, next: UInt) => {
					next := RegNext(prev >> stageWidth)
					next
				})
				
		val Cin = Seq.fill(stages)(Wire(UInt(1.W)))//creating carry in wires
		
		val adderResults = Seq.tabulate(stages)(i => singleStageAdd(A(i), B(i), Cin(i)))//connecting inputs to adder
		
		Cin.zip(io.Cin +: adderResults.map(_(stageWidth))).
		foreach{case(carryIn: UInt, carryRes: UInt) => carryIn := RegNext(carryRes)}//connecting carry-outs to carry-ins
		
		val sum = Cat((0 until stages).map(i => stages - i).zip(adderResults.map(_(stageWidth - 1, 0))).
					map{case(delay, result) => ShiftRegister(result, delay)}.reverse)//getting the final result
		
		val overflow = RegNext(adderResults(stages - 1)(stageWidth)) 
		
		io.sum := Cat(overflow, sum)
		
	}
	
	private def getSum(addUnit: Adder)(A: UInt, B: UInt, Cin: UInt) = {
		addUnit.io.A := A
		addUnit.io.B := B
		addUnit.io.Cin := Cin
		addUnit.io.sum
	}
	
	def generateAdder(addUnit: add)(A: UInt, B: UInt, Cin:UInt) = addUnit match {
		case cla_add(width, withOverFlow) => 
			getSum(Module(new singleCycleAdder(width, withOverFlow) with cla))(A, B, Cin)
		case ripple_add(width, withOverFlow) =>
			getSum(Module(new singleCycleAdder(width, withOverFlow) with ripple))(A, B, Cin)
		case pipelined_add_with_cla(stages, width, withOverFlow) =>
			getSum(Module(new pipelined_adder(stages, width, withOverFlow)))(A, B, Cin)
		}

}

class generate_adder(width: Int, withOverFlow: Boolean) extends Adder(width, withOverFlow) {
	io.sum := Adder.generateAdder(cla_add(width, withOverFlow))(io.A, io.B, io.Cin)
}

object generate_adder extends App {
    (new chisel3.stage.ChiselStage).emitVerilog(new generate_adder(16, true))
}
