import chisel3._
import chisel3.util._
import chisel3.Driver

package adders

abstract class adder(width: Int, withOverFlow: Boolean) extends Module {
	val io = IO(new Bundle{
		val A = Input(UInt(width.W))
		val B = Input(UInt(width.W))
		val Cin = Input(UInt(1.W))
		val sum = Output(UInt((if (withOverFlow) (width+1) else width).W))
	})
}

object adder {

	private class cla_adder(width: Int, withOverFlow: Boolean) extends adder(width, withOverFlow) {
		val P = io.A ^ io.B
		val G = io.A & io.B
		
		def getPartialProduct(carry_index: Int)(product_index: Int) = 
			if (carry_index == 0) io.Cin
			else if (product_index == 0) (P(carry_index - 1, 0).andR & io.Cin)
			else if (product_index == carry_index) G(carry_index - 1)
			else (P(carry_index - 1, product_index).andR & G(product_index - 1))
				
		def partialProducts(carry_index: Int) = 
			Seq.tabulate(carry_index + 1)(getPartialProduct(carry_index)_)

		val C = Cat(Seq.tabulate(width+1)(i => partialProducts(i).reduce(_ | _)).reverse)
			
		io.sum := C | io.A | io.B
	}
	
	private class pipelined_adder(stages: Int, width: Int, withOverFlow: Boolean) extends adder(width, withOverFlow) {
		val stageWidth = width/stages									//adder width for a single pipeline stage
		val singleStageAdd = generateAdder(stageWidth, withOverFlow)_	//adder for the pipeline width
		
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
	
	private def getSum(addUnit: adder)(A: UInt, B: UInt, Cin: UInt) = {
		addUnit.io.A := A
		addUnit.io.B := B
		addUnit.io.Cin := Cin
		addUnit.io.sum
	}
	
	def generateAdder(width: Int, withOverFlow: Boolean)(A: UInt, B: UInt, Cin:UInt) = 
		getSum( Module(new cla_adder(width, withOverFlow)) )(A, B, Cin)
	
	def generateAdder(stages: Int, width: Int, withOverFlow: Boolean)(A: UInt, B: UInt, Cin:UInt) = 
		getSum( Module(new pipelined_adder(stages, width, withOverFlow)) )(A, B, Cin)

}

class generate_adder(width: Int, withOverFlow: Boolean) extends adder(width, withOverFlow) {
	io.sum := adder.generateAdder(width, withOverFlow)(io.A, io.B, io.Cin)
}

object generate_adder extends App {
    (new chisel3.stage.ChiselStage).emitVerilog(new generate_adder(16, true))
}
