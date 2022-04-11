/********
* Assembler.scala
* Author: <ENTER YOUR NAMES HERE>
* Date: Fall 2017, CSC110
*
* Implements a very simple interpreter for an assembler-like language
* called LC-q (a variant of LC-3)
*********/

import scala.io.StdIn._

// The Machine State
var program: List[List[String]] = List()          // The program itself
var programCounter: Int = 0                       // Where in the program are we?
// Include other machine state variables like memory, registers, conditionCode...
var conditionCode: Int = 0
val registers: Array[Int] = Array.fill(8)(0)
val memory = new Array[Int](1024)
var m = Map[String,Int]()
var x =0
// Report an error and TERMINATE
def errorExit(msg: String) {
  println("ERROR: " + msg)
  sys.exit(0)  // This terminates the program
}

// Execute the print operation
def executePrint(line: List[String]) {
    // Get the register to println
    if (line.length != 2) errorExit("INVALID INSTRUCTION")
    val reg = line(1)(1) - '0' // Convert register Character to a number
    println(registers(reg))
 }

 def executeLDI(line: List[String])
 {
   if (line.length != 3) errorExit("INVALID INSTRUCTION")
   val a = line(1)(1) - '0'
   val v = line(2).toInt
   registers(a) = v
   conditionCode = registers(a)
 }

 def executeLDA(line: List[String])
 {
   if (line.length != 3) errorExit("INVALID INSTRUCTION")
   val b = line(2)(1) - '0'
   val a = line(1)(1) - '0'
   if (registers(b) > 1023 ||  registers(b) < 0)
   {
      errorExit("MEMORY ADDRESS IS NOT IN RANGE.")
   }
   registers(a) = memory(registers(b))
   conditionCode = registers(a)
 }

// Process the current line based on Program Counter
def processCurrentLine() {
  // Check out the line
  val line = program(programCounter)
  line.head match {
    case "PRINT" => executePrint(line)
    case "LDI" => executeLDI(line)
    case "LDA" => executeLDA(line)
    case "STORE" => executeStore(line)
    case "ADD" => executeAdd(line)
    case "JMP" => executejmp(line)
    case "BRZ" => executeBRZ(line)
    case "BRP" => executeBRP(line)
    case "BRN" => executeBRN(line)
    case "MULT" => executeMult(line)
    case "AND" => executeAnd(line)
    case "XOR" => executeXor(line)
    case "LABEL" =>
    case _       => errorExit("INVALID INSTRUCTION" + programCounter) // Or not yet recognized
  }
  programCounter += 1 // Move on to the next line
}
// Read in the program
def readProgram(): List[List[String]] = {
  val line = readLine()   // Get current line
  if (line == null)
    // End of input reached
    List()
  else {
    // Read rest of Program and attach this line (parsed by spaces) to it
    val restOfProgram = readProgram()  // Read rest of List
    line.toUpperCase.split(" ").toList::restOfProgram
  }
}


def executeStore(line: List[String]) {
if (line.length != 3) errorExit("INVALID INSTRUCTION")
val b = line(2)(1) - '0'
val a = line(1)(1) - '0'
if (registers(b) > 1023 ||  registers(b) < 0)
{
   errorExit("MEMORY ADDRESS IS NOT IN RANGE.")
}
  memory(registers(b)) = registers(a)
}


// Print out the entire program (debugging purposes really)
def printProgram(program: List[List[String]]) {
  var lineNo = 0
  for (line <- program) {
    println(f"$lineNo:\t ${line.mkString("\t")}")
    lineNo += 1  // Increment the line number
  }
}

def executeAdd(line: List[String])
{

  val a = line(1)(1) - '0'
  val b = line(2)(1) - '0'
  val c = line(3)(1) - '0'
  registers(a) = registers(b) + registers(c)
  conditionCode = registers(a)

  if (line.length != 4) {
    errorExit("INVALID INSTRUCTION")
  }
}

def executeMult(line: List[String])
{
  if (line.length != 4) {
    errorExit("INVALID INSTRUCTION")
  }
  val a = line(1)(1) - '0'
  val b = line(2)(1) - '0'
  val c = line(3)(1) - '0'
  registers(a) = registers(b) * registers(c)
  conditionCode = registers(a)
}

def executeAnd(line: List[String])
{
  if (line.length != 4) {
    errorExit("INVALID INSTRUCTION")
  }
  val a = line(1)(1) - '0'
  val b = line(2)(1) - '0'
  val c = line(3)(1) - '0'
  registers(a) = (registers(b) & registers(c)) 
  conditionCode = registers(a)
}

def executeXor(line: List[String])
{
  if (line.length != 4) {
    errorExit("INVALID INSTRUCTION")
  }
  val a = line(1)(1) - '0'
  val b = line(2)(1) - '0'
  val c = line(3)(1) - '0'
  registers(a) = registers(b) ^ registers(c)
  conditionCode = registers(a)
}

// Determine if the program is finished (programCounter is beyond end of program!)
def programFinished(): Boolean = programCounter == program.length

// Main body (load and preprocess the program)
program = readProgram()
label()
// Just a debug step (remove at some point!)
// println("DEBUG: Here is the program")
// println("==========================")
// printProgram(program)
// println("==========================")

// Identify all of the labels (useful to do this before hand for branching)
// ...
def label() ={
   for (i <- 0 until program.length){
     if (program(i)(0)== "LABEL") {
       m= m+ (program(i)(1)->programCounter)
     }
   }
}
def executejmp(line: List[String]) {
    if (line.length != 2) {errorExit("INVALID INSTRUCTION")}
    programCounter=m(line(1))
}
def executeBRZ(line: List[String]) {
  if (line.length != 2) {errorExit("INVALID INSTRUCTION")}
  if (conditionCode==0) {
    programCounter=m(line(1))
  }
}
def executeBRP(line: List[String]) {
  if (line.length != 2) {errorExit("INVALID INSTRUCTION")}
  if (conditionCode>0) {
    programCounter=m(line(1))
  }
}
def executeBRN(line: List[String]) {
  if (line.length != 2) {errorExit("INVALID INSTRUCTION")}
  if (conditionCode<0) {
     programCounter=m(line(1))
  }
}


// Run the program starting at line 0
programCounter = 0
while (!programFinished()) {
  processCurrentLine()
}
