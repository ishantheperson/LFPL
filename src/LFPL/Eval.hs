module LFPL.Eval where 

import LFPL.AST 

data LFPLValue = 
    LFPLBoolValue Bool
  | LFPLIntValue Integer 
  | LFPLUnitValue
  | LFPLDiamondValue
  | LFPLFunctionValue (LFPLValue -> LFPLValue)
  | LFPLPairValue (LFPLValue, LFPLValue)
  | LFPLListValue [LFPLValue]

