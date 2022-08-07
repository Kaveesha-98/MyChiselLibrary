package adders

import chisel3._
import chisel3.util._
import chisel3.Driver

sealed trait add
case class cla_add(width: Int, withOverFlow: Boolean) extends add
case class ripple_add(width: Int, withOverFlow: Boolean) extends add
case class pipelined_add_with_cla(stages: Int, width: Int, withOverFlow: Boolean) extends add
case class pipelined_add_with_ripple(stages: Int, width: Int, withOverFlow: Boolean) extends add

abstract class Adder(width: Int, withOverFlow: Boolean) extends Module {
	val io = IO(new Bundle{
		val A = Input(UInt(width.W))
		val B = Input(UInt(width.W))
		val Cin = Input(UInt(1.W))
		val sum = Output(UInt((if (withOverFlow) (width+1) else width).W))
	})
}

object Adder {

	private abstract class SingleCycleAdder(val width: Int, val withOverFlow: Boolean) extends Adder(width, withOverFlow) {
	
		val P = io.A ^ io.B
		val G = io.A & io.B
		
		//From ripple to carry-look-ahead only the way of calculating the carry changes
		def C: UInt
		
		io.sum := C ^ P
		
	}
	
	private trait CarryLookAhead extends SingleCycleAdder{
	
		def getPartialProduct(carry_index: Int)(product_index: Int) = 
			if (carry_index == 0) io.Cin
			else if (product_index == 0) (P(carry_index - 1, 0).andR & io.Cin)
			else if (product_index == carry_index) G(carry_index - 1)
			else (P(carry_index - 1, product_index).andR & G(product_index - 1))
				
		def partialProducts(carry_index: Int) = 
			Seq.tabulate(carry_index + 1)(getPartialProduct(carry_index)_)

		override def C = Cat(Seq.tabulate(width+1)(i => partialProducts(i).reduce(_ | _)).reverse)
	
	}
	
	private trait Ripple extends SingleCycleAdder{
		
		override def C = Cat((Seq.tabulate(width)(P(_))).zip(Seq.tabulate(width)(G(_))).
				scanLeft(io.Cin)((carryIn: UInt, PG: (UInt, UInt)) => PG._2 | (PG._1 & carryIn)).reverse)
	
	}
	
	private abstract class pipelinedAdder(stages: Int, width: Int, val withOverFlow: Boolean) extends Adder(width, withOverFlow) {
	
		//addition width in each stage
		val stageWidth = width/stages
		//this will be the addition used for addition in the pipeline stage
		def singleStageAdd: (UInt, UInt, UInt) => UInt
		
		//source for addition will move on to the next pipeline stage until addition is finished
		val A = Seq.tabulate(stages-1)(i => Wire(UInt((width - stageWidth*(i+1)).W))).
				scan(RegNext(io.A))((prev: UInt, next: UInt) => {
					next := RegNext(prev >> stageWidth)
					next
				})
				
		//source for addition will move on to the next pipeline stage until addition is finished
		val B = Seq.tabulate(stages-1)(i => Wire(UInt((width - stageWidth*(i+1)).W))).
				scan(RegNext(io.B))((prev: UInt, next: UInt) => {
					next := RegNext(prev >> stageWidth)
					next
				})
				
		//wires to connect the overflow of addition in each stage
		val Cin = Seq.fill(stages)(Wire(UInt(1.W)))
		
		//adder results in each stage
		val adderResults = Seq.tabulate(stages)(i => singleStageAdd(A(i), B(i), Cin(i)))
		
		//connecting the carry-outs to carry-ins in the next stage
		Cin.zip(io.Cin +: adderResults.map(_(stageWidth))).
		foreach{case(carryIn: UInt, carryRes: UInt) => carryIn := RegNext(carryRes)}
		
		//getting the answer, result from each stage is delayed(different for each stage) to get final answer
		val sum = Cat((0 until stages).map(i => stages - i).zip(adderResults.map(_(stageWidth - 1, 0))).
					map{case(delay, result) => ShiftRegister(result, delay)}.reverse)
		
		//overflow bit from the last stage
		val overflow = RegNext(adderResults(stages - 1)(stageWidth)) 
		
		io.sum := Cat(overflow, sum)
		
	}
	
	private trait CarryLookAheadStage extends pipelinedAdder{
	
		override def singleStageAdd = generateAdder(cla_add(stageWidth, withOverFlow))_
	
	}
	
	private trait RippleStage extends pipelinedAdder{
	
		override def singleStageAdd = generateAdder(ripple_add(stageWidth, withOverFlow))_	
		
	}
	
	private def getSum(addUnit: Adder)(A: UInt, B: UInt, Cin: UInt) = {
		addUnit.io.A := A
		addUnit.io.B := B
		addUnit.io.Cin := Cin
		addUnit.io.sum
	}
	
	def generateAdder(addUnit: add)(A: UInt, B: UInt, Cin:UInt) = addUnit match {
		case cla_add(width, withOverFlow) => 
			getSum(Module(new SingleCycleAdder(width, withOverFlow) with CarryLookAhead))(A, B, Cin)
		case ripple_add(width, withOverFlow) =>
			getSum(Module(new SingleCycleAdder(width, withOverFlow) with Ripple))(A, B, Cin)
		case pipelined_add_with_cla(stages, width, withOverFlow) =>
			getSum(Module(new pipelinedAdder(stages, width, withOverFlow) with CarryLookAheadStage))(A, B, Cin)
		case pipelined_add_with_ripple(stages, width, withOverFlow) =>
			getSum(Module(new pipelinedAdder(stages, width, withOverFlow) with RippleStage))(A, B, Cin)
		}

}

class generate_adder(width: Int, withOverFlow: Boolean) extends Adder(width, withOverFlow) {
	io.sum := Adder.generateAdder(pipelined_add_with_ripple(2, width, withOverFlow))(io.A, io.B, io.Cin)
}

object generate_adder extends App {
    (new chisel3.stage.ChiselStage).emitVerilog(new generate_adder(16, true))
}
