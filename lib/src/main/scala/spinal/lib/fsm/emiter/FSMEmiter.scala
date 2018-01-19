package spinal.lib.fsm.emiter

import spinal.core._
import spinal.core.internals._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer

case class FSMEmiterTransition(start: State, end: State, scope: ScopeStatement){
  override def toString: String = s"${start} ---> ${end}"
}
case class FSMEmiterInfo(
  transition: ArrayBuffer[FSMEmiterTransition] = new ArrayBuffer[FSMEmiterTransition]()
)


abstract class FSMEmiter {

  def getNameState(state: State): String = {

    val name = state match{
      case s: StateDelay         => s"${s.getName()}_delay"
      case s: StateBoot          => if(s.autoStart) "boot" else "exit"
      case s: StateParallelFsm   => s"${s.getName()}_//"
      case s: StateFsm[_]        => s"${s.getName()}_fsm"
      case s: State              => s.getName()
      case _                     => "always"
    }

    return s"${"\"" + name + "\""}"
  }


  def getConditionStrFromScope(scope: ScopeStatement): String = {
    scope.parentStatement match{
      case x: WhenStatement if scope == x.whenTrue  => dispatchExpression(x.cond)
      case x: WhenStatement if scope == x.whenFalse => s"!(${dispatchExpression(x.cond)})"
      case x: SwitchStatement if x.parentScope.parentStatement != null =>
        val element = x.elements.filter(elem => elem.scopeStatement == scope)
        val cond = if(element.isEmpty) s"default" else s"${dispatchExpression(element.head.keys.head)}"
        s"${dispatchExpression(x.value)} / $cond"
      case _ => ""
    }
  }

  def dispatchExpression(e: Expression):  String = e match {
    case  e: BaseType                                => s"${e.getName()}"

    case  e: BoolLiteral                             => s"${e.value}"
    case  e: BitsLiteral                             => s"${e.value}"
    case  e: UIntLiteral                             => s"${e.value}"
    case  e: SIntLiteral                             => s"${e.value}"
    case  e: EnumLiteral[_]                          => s"${e.enum.getName()}"

    case  e: BoolPoison                              => ""
    case  e: EnumPoison                              => ""

    //unsigned
    case  e: Operator.UInt.Add                       => s"(${dispatchExpression(e.left)} + ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.Sub                       => s"(${dispatchExpression(e.left)} - ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.Mul                       => s"(${dispatchExpression(e.left)} * ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.Div                       => s"(${dispatchExpression(e.left)} / ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.Mod                       => s"(${dispatchExpression(e.left)} % ${dispatchExpression(e.right)})"

    case  e: Operator.UInt.Or                        => s"(${dispatchExpression(e.left)} | ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.And                       => s"(${dispatchExpression(e.left)} & ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.Xor                       => s"(${dispatchExpression(e.left)} ^ ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.Not                       => s"(~${dispatchExpression(e.source)})"

    case  e: Operator.UInt.Equal                     => s"(${dispatchExpression(e.left)} == ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.NotEqual                  => s"(${dispatchExpression(e.left)} != ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.Smaller                   => s"(${dispatchExpression(e.left)} < ${dispatchExpression(e.right)})"
    case  e: Operator.UInt.SmallerOrEqual            => s"(${dispatchExpression(e.left)} <= ${dispatchExpression(e.right)})"

    case  e: Operator.UInt.ShiftRightByInt           => s"${dispatchExpression(e.source)} >> ${e.shift}"
    case  e: Operator.UInt.ShiftLeftByInt            => s"${dispatchExpression(e.source)} << ${e.shift}"
    case  e: Operator.UInt.ShiftRightByUInt          => s"${dispatchExpression(e.left)} >> ${dispatchExpression(e.right)}"
    case  e: Operator.UInt.ShiftLeftByUInt           => s"${dispatchExpression(e.left)} << ${dispatchExpression(e.right)}"
    case  e: Operator.UInt.ShiftRightByIntFixedWidth => s"${dispatchExpression(e.source)} >> ${e.shift}"
    case  e: Operator.UInt.ShiftLeftByIntFixedWidth  => s"${dispatchExpression(e.source)} << ${e.shift}"
    case  e: Operator.UInt.ShiftLeftByUIntFixedWidth => s"${dispatchExpression(e.left)} << ${dispatchExpression(e.right)}"

    //signed
    case  e: Operator.SInt.Add                       => s"(${dispatchExpression(e.left)} + ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.Sub                       => s"(${dispatchExpression(e.left)} - ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.Mul                       => s"(${dispatchExpression(e.left)} * ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.Div                       => s"(${dispatchExpression(e.left)} / ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.Mod                       => s"(${dispatchExpression(e.left)} % ${dispatchExpression(e.right)})"

    case  e: Operator.SInt.Or                        => s"(${dispatchExpression(e.left)} | ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.And                       => s"(${dispatchExpression(e.left)} & ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.Xor                       => s"(${dispatchExpression(e.left)} ^ ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.Not                       => s"(~${dispatchExpression(e.source)})"
    case  e: Operator.SInt.Minus                     => s"(-${dispatchExpression(e.source)})"

    case  e: Operator.SInt.Equal                     => s"(${dispatchExpression(e.left)} == ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.NotEqual                  => s"(${dispatchExpression(e.left)} != ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.Smaller                   => s"(${dispatchExpression(e.left)} < ${dispatchExpression(e.right)})"
    case  e: Operator.SInt.SmallerOrEqual            => s"(${dispatchExpression(e.left)} <= ${dispatchExpression(e.right)})"


    case  e: Operator.SInt.ShiftRightByInt           => s"${dispatchExpression(e.source)} >> ${e.shift}"
    case  e: Operator.SInt.ShiftLeftByInt            => s"${dispatchExpression(e.source)} << ${e.shift}"
    case  e: Operator.SInt.ShiftRightByUInt          => s"${dispatchExpression(e.left)} >> ${dispatchExpression(e.right)}"
    case  e: Operator.SInt.ShiftLeftByUInt           => s"${dispatchExpression(e.left)} << ${dispatchExpression(e.right)}"
    case  e: Operator.SInt.ShiftRightByIntFixedWidth => s"${dispatchExpression(e.source)} >> ${e.shift}"
    case  e: Operator.SInt.ShiftLeftByIntFixedWidth  => s"${dispatchExpression(e.source)} << ${e.shift}"
    case  e: Operator.SInt.ShiftLeftByUIntFixedWidth => s"${dispatchExpression(e.left)} << ${dispatchExpression(e.right)}"

    //bits
    case  e: Operator.Bits.Cat                       => s"Cat(${dispatchExpression(e.left)}, ${dispatchExpression(e.right)})"

    case  e: Operator.Bits.Or                        => s"(${dispatchExpression(e.left)} | ${dispatchExpression(e.right)})"
    case  e: Operator.Bits.And                       => s"(${dispatchExpression(e.left)} & ${dispatchExpression(e.right)})"
    case  e: Operator.Bits.Xor                       => s"(${dispatchExpression(e.left)} ^ ${dispatchExpression(e.right)})"
    case  e: Operator.Bits.Not                       => s"(~${dispatchExpression(e.source)})"

    case  e: Operator.Bits.Equal                     => s"(${dispatchExpression(e.left)} == ${dispatchExpression(e.right)})"
    case  e: Operator.Bits.NotEqual                  => s"(${dispatchExpression(e.left)} != ${dispatchExpression(e.right)})"

    case  e: Operator.Bits.ShiftRightByInt           => s"${dispatchExpression(e.source)} >> ${e.shift}"
    case  e: Operator.Bits.ShiftLeftByInt            => s"${dispatchExpression(e.source)} << ${e.shift}"
    case  e: Operator.Bits.ShiftRightByUInt          => s"${dispatchExpression(e.left)} >> ${dispatchExpression(e.right)}"
    case  e: Operator.Bits.ShiftLeftByUInt           => s"${dispatchExpression(e.left)} << ${dispatchExpression(e.right)}"
    case  e: Operator.Bits.ShiftRightByIntFixedWidth => s"${dispatchExpression(e.source)} >> ${e.shift}"
    case  e: Operator.Bits.ShiftLeftByIntFixedWidth  => s"${dispatchExpression(e.source)} << ${e.shift}"
    case  e: Operator.Bits.ShiftLeftByUIntFixedWidth => s"${dispatchExpression(e.left)} << ${dispatchExpression(e.right)}"

    //bool
    case  e: Operator.Bool.Equal                     => s"(${dispatchExpression(e.left)} == ${dispatchExpression(e.right)})"
    case  e: Operator.Bool.NotEqual                  => s"(${dispatchExpression(e.left)} != ${dispatchExpression(e.right)})"

    case  e: Operator.Bool.Not                       => s"(!${dispatchExpression(e.source)})"
    case  e: Operator.Bool.And                       => s"(${dispatchExpression(e.left)} & ${dispatchExpression(e.right)})"
    case  e: Operator.Bool.Or                        => s"(${dispatchExpression(e.left)} | ${dispatchExpression(e.right)})"
    case  e: Operator.Bool.Xor                       => s"(${dispatchExpression(e.left)} ^ ${dispatchExpression(e.right)})"


    //enum
    case  e: Operator.Enum.Equal                     => s"(${dispatchExpression(e.left)} == ${dispatchExpression(e.right)})"
    case  e: Operator.Enum.NotEqual                  => s"(${dispatchExpression(e.left)} != ${dispatchExpression(e.right)})"

    //cast
    case  e: CastSIntToBits                          => s"${dispatchExpression(e.input)}"
    case  e: CastUIntToBits                          => s"${dispatchExpression(e.input)}"
    case  e: CastBoolToBits                          => s"${dispatchExpression(e.input)}"
    case  e: CastEnumToBits                          => s"${dispatchExpression(e.input)}"

    case  e: CastBitsToSInt                          => s"${dispatchExpression(e.input)}"
    case  e: CastUIntToSInt                          => s"${dispatchExpression(e.input)}"

    case  e: CastBitsToUInt                          => s"${dispatchExpression(e.input)}"
    case  e: CastSIntToUInt                          => s"${dispatchExpression(e.input)}"

    case  e: CastBitsToEnum                          => s"${dispatchExpression(e.input)}"
    case  e: CastEnumToEnum                          => s"${dispatchExpression(e.input)}"

    //misc
    case  e: ResizeSInt                              => s"${dispatchExpression(e.input)}"
    case  e: ResizeUInt                              => s"${dispatchExpression(e.input)}"
    case  e: ResizeBits                              => s"${dispatchExpression(e.input)}"

    case  e: BinaryMultiplexer                       => s"$Mux(${dispatchExpression(e.cond)},${dispatchExpression(e.whenTrue)},${dispatchExpression(e.whenFalse)})"

    case  e: BitVectorBitAccessFixed                 => s"(${dispatchExpression(e.source)}(${e.bitId}))"
    case  e: BitVectorBitAccessFloating              => s"(${dispatchExpression(e.source)}(${e.bitId}))"
    case  e: BitVectorRangedAccessFixed              => s"(${dispatchExpression(e.source)}(${e.hi} downto ${e.lo}))"
    case  e: BitVectorRangedAccessFloating           => s"(${dispatchExpression(e.source)}(${dispatchExpression(e.offset)},${e.size}))"
  }

}
