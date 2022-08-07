This code is written to generate 4 different types of adders. 
* A single cycle carry-look-ahead adder
* A single cycle ripple adder
* A pipelined adder with carry-look-ahead adders
* A pipelined adder with ripple adders

## Implementation of the Modules
![adder](https://user-images.githubusercontent.com/58455731/183308218-108a30ee-18cf-4d4b-b578-ba39e71903b3.png)
* Adder
  * Base module that extends to create adder modules. All modules have the same i/o. This class defines the i/o with the variables
* SingleCycleAdder
  * Base module that extends to create the single cycle adder. Both ripple adder and the carry look ahead adder have the same combinational logic for propagate bit and generate bit. Carry bit logic differs in the two modules. Which has been left undefined, to be defined in extended traits
* PipelinedAdder
  * This extends to pipelined adder with a carry-look-ahead adder or with a ripple adder to do additions in a pipelinestage.

## Generating the modules
```
Module(new SingleCycleAdder(width, withOverFlow) with CarryLookAhead) // Carry-look-ahead single cycle adder
Module(new SingleCycleAdder(width, withOverFlow) with Ripple)         // Ripple adder single cycle
Module(new pipelinedAdder(stages, width, withOverFlow) with CarryLookAheadStage) // pipelined adder with carry look ahead add pipeline stages
Module(new pipelinedAdder(stages, width, withOverFlow) with RippleStage)         // pipelined adder with ripple add pipeline stages
```

## Calling a adder in a hardware module
All the defined modules are private to object `Adder`. A function is provided by `Adder` to generate a adder in the code
```
val result = generateAdder(adder_type)(sources)
```
### adder_type
Defines the type of adder needed by your code. This done through predefined case classes that extends the add class(which extends Any in scala)
```
sealed trait add
case class cla_add(width: Int, withOverFlow: Boolean) extends add
case class ripple_add(width: Int, withOverFlow: Boolean) extends add
case class pipelined_add_with_cla(stages: Int, width: Int, withOverFlow: Boolean) extends add
case class pipelined_add_with_ripple(stages: Int, width: Int, withOverFlow: Boolean) extends add
```
### sources
Defines the sources for addition

### usage example(A pipelined adder with carry-look-ahead adders in pipeline stages, 4 pipelined stages, 32 bit wide and with overflow)
```
val result = Adder.generateAdder(pipelined_add_with_ripple(4, 32, true))(A, B, carryBit)
```
