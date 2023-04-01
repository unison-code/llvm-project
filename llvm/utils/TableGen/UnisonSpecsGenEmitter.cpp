//===- UnisonSpecsGenEmitter.cpp - Unison SpecsGen Generator ----*- C++ -*-==//
//
//  Main authors:
//    Jan Tomljanovic <jan.tomljanovic@sics.se>
//    Roberto Castaneda Lozano <rcas@acm.org>
//
//  This file is part of Unison, see http://unison-code.github.io
//
//  Copyright (c) 2016, RISE SICS AB
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are met:
//  1. Redistributions of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//  3. Neither the name of the copyright holder nor the names of its
//     contributors may be used to endorse or promote products derived from this
//     software without specific prior written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
//  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
//  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
//  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
//  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
//  POSSIBILITY OF SUCH DAMAGE.
//
//===----------------------------------------------------------------------===//

#include "CodeGenTarget.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include <iomanip>
#include <sstream>
#include <vector>
using namespace llvm;

#define DEBUG_TYPE "unison-specsgen-emitter"

// An Operand can be a Register, Label or a Bound (any other Operand that is not
// interpreted by Unison, such as immediates). If it is a register, UseDef and
// RegType are defined.
namespace unison {

struct Operand {
  enum { Register, Label, Bound } Type;
  std::string Name;
  std::string UseDef;
  std::string RegType;
};

} // end namespace unison

typedef std::pair<std::string, std::string> StringPair;
typedef std::vector<StringPair> StringPairVector;
typedef std::vector<std::string> StringVector;
typedef std::vector<unison::Operand> OperandVector;

/// Instruction with methods to be printed in .yaml format.
class Instruction {
private:
  std::string Id;
  std::string Type;
  OperandVector Operands;
  StringVector Uses;
  StringVector Defs;
  int Size;
  bool AffectsMem;
  bool AffectedMem;
  StringVector AffectsReg;
  StringVector AffectedReg;
  std::string Itinerary;

  void printAffs(llvm::raw_ostream &OS, std::string Name, bool Memory,
                 StringVector Regs);
  void printUseDefs(llvm::raw_ostream &OS, StringVector UseDefs,
                    std::string Name);
  void printAttribute(std::string Name, std::string Value,
                      llvm::raw_ostream &OS);
  void printField(std::string Name, std::string Value, llvm::raw_ostream &OS);

public:
  Instruction(std::string Id, std::string Type, OperandVector Operands,
              StringVector Uses, StringVector Defs, int Size, bool AffectsMem,
              bool AffectedMem, StringVector AffectsReg,
              StringVector AffectedReg, std::string Itinerary);
  void printId(llvm::raw_ostream &OS);
  void printType(llvm::raw_ostream &OS);
  void printOperands(llvm::raw_ostream &OS);
  void printUses(llvm::raw_ostream &OS);
  void printDefs(llvm::raw_ostream &OS);
  void printSize(llvm::raw_ostream &OS);
  void printAffects(llvm::raw_ostream &OS);
  void printAffected(llvm::raw_ostream &OS);
  void printItinerary(llvm::raw_ostream &OS);
  void printAll(llvm::raw_ostream &OS);
};

Instruction::Instruction(std::string Id0, std::string Type0,
                         OperandVector Operands0, StringVector Uses0,
                         StringVector Defs0, int Size0, bool AffectsMem0,
                         bool AffectedMem0, StringVector AffectsReg0,
                         StringVector AffectedReg0, std::string Itinerary0)
    : Id(Id0), Type(Type0), Operands(Operands0), Uses(Uses0), Defs(Defs0),
      Size(Size0), AffectsMem(AffectsMem0), AffectedMem(AffectedMem0),
      AffectsReg(AffectsReg0), AffectedReg(AffectedReg0),
      Itinerary(Itinerary0){}

void Instruction::printId(llvm::raw_ostream &OS) {
  std::stringstream Buffer;
  Buffer << std::setw(8) << " " << std::setw(22) << std::left << "- id:" << Id
         << '\n';
  OS << Buffer.str();
}

void Instruction::printType(llvm::raw_ostream &OS) {
  printAttribute("type:", Type, OS);
}

void Instruction::printOperands(llvm::raw_ostream &OS) {
  std::stringstream Buffer;
  Buffer << std::setw(10) << " "
         << "operands:" << '\n';
  OS << Buffer.str();
  for (unison::Operand Op : Operands) {
    std::string Value;
    switch (Op.Type) {
    case unison::Operand::Label:
      Value = "label";
      break;
    case unison::Operand::Bound:
      Value = "bound";
      break;
    case unison::Operand::Register:
      Value = "[register, " + Op.UseDef + ", " + Op.RegType + "]";
      break;
    }
    printField(Op.Name, Value, OS);
  }
}

void Instruction::printUseDefs(llvm::raw_ostream &OS, StringVector UseDefs,
                               std::string Name) {
  std::string Value = "[";
  std::string Sep = "";
  for (std::string UseDef : UseDefs) {
    Value += (Sep + UseDef);
    Sep = ", ";
  }
  Value += "]";
  printAttribute(Name + ":", Value, OS);
}

void Instruction::printUses(llvm::raw_ostream &OS) {
  printUseDefs(OS, Uses, "uses");
}

void Instruction::printDefs(llvm::raw_ostream &OS) {
  printUseDefs(OS, Defs, "defines");
}

void Instruction::printSize(llvm::raw_ostream &OS) {
  printAttribute("size:", std::to_string(Size), OS);
}

void Instruction::printAffects(llvm::raw_ostream &OS) {
  printAffs(OS, "affects", AffectsMem, AffectsReg);
}

void Instruction::printAffected(llvm::raw_ostream &OS) {
  printAffs(OS, "affected-by", AffectedMem, AffectedReg);
}

void Instruction::printAffs(llvm::raw_ostream &OS, std::string Name,
                            bool Memory, StringVector Regs) {
  std::stringstream Buffer;
  Buffer << std::setw(10) << " " << Name + ":" << '\n';
  OS << Buffer.str();
  if (Memory)
    printField("mem", "memory", OS);
  for (std::string Reg : Regs)
    printField(Reg, "register", OS);
}

void Instruction::printItinerary(llvm::raw_ostream &OS) {
  printAttribute("itinerary:", Itinerary, OS);
}

void Instruction::printAll(llvm::raw_ostream &OS) {
  OS << "\n";
  Instruction::printId(OS);
  Instruction::printType(OS);
  Instruction::printOperands(OS);
  Instruction::printUses(OS);
  Instruction::printDefs(OS);
  Instruction::printSize(OS);
  Instruction::printAffects(OS);
  Instruction::printAffected(OS);
  Instruction::printItinerary(OS);
}

/// Prints a simple attribute.
void Instruction::printAttribute(std::string Name, std::string Value,
                                 llvm::raw_ostream &OS) {
  std::stringstream Buffer;
  if (Value.empty())
    Buffer << std::setw(10) << " " << Name << '\n';
  else
    Buffer << std::setw(10) << " " << std::setw(20) << std::left << Name
           << Value << '\n';
  OS << Buffer.str();
}

/// Prints the subelements of a complex attribute.
void Instruction::printField(std::string Name, std::string Value,
                             llvm::raw_ostream &OS) {
  std::string Name1 = "- " + Name + ": ";
  std::stringstream Buffer;
  Buffer << std::setw(11) << " " << std::setw(19) << std::left << Name1 << Value
         << '\n';
  OS << Buffer.str();
}

namespace {
class UnisonSpecsGenEmitter {

  RecordKeeper &Records;
  CodeGenTarget Target;

  StringVector flat(Record *Rec);
  void printYaml(std::vector<Instruction> Instructions, raw_ostream &OS);
  std::string getRecordItinerary(Record *Rec);
  StringVector getRegisterList(std::string Field, Record *Rec);
  bool getRecordBool(Record *Rec, std::string Field, bool Def);
  int getRecordSize(Record *Rec);
  StringPairVector *parseOperands(std::string Field, Record *Rec);
  StringVector getNames(StringPairVector *List);
  void executeConstraints(StringPairVector *Outs, std::string Cons);
  OperandVector getOperands(StringPairVector *Outs, StringPairVector *ins,
                            RecordKeeper &Records);
  void getOperandsFromVector(StringPairVector *Vec, StringPairVector *Help,
                             OperandVector *Operands, bool Defs,
                             RecordKeeper &Records);
  bool isRegister(Record *Rec);
  bool isLabel(Record *Rec);
  std::string getRecordType(Record *Rec);
  std::string getRecordId(Record *Rec);
  bool fieldExists(Record *Rec, std::string Field);
  bool allNeededFieldsExist(Record *Rec);
  StringVector split(std::string Str, char Del);
  std::string trim(std::string Str);
  std::string escape(std::string Name);

public:
  UnisonSpecsGenEmitter(RecordKeeper &R) : Records(R), Target(R) {}

  /// run - Output Unison's SpecsGen processor description in YAML format.

  void run(raw_ostream &OS);
};
} // End anonymous namespace

void UnisonSpecsGenEmitter::run(raw_ostream &OS) {
  std::vector<Instruction> Instructions;
  for (const auto &D : Records.getDefs()) {
    Record *Rec = &(*D.second);
    if (!allNeededFieldsExist(Rec))
      continue;
    std::string Id = getRecordId(Rec);
    std::string Type = getRecordType(Rec);
    StringPairVector *OutList = parseOperands("OutOperandList", Rec);
    StringPairVector *InList = parseOperands("InOperandList", Rec);
    executeConstraints(OutList, Rec->getValueAsString("Constraints").str());
    StringVector Uses = getNames(InList);
    StringVector Defs = getNames(OutList);
    OperandVector Operands = getOperands(OutList, InList, Records);
    int Size = getRecordSize(Rec);
    bool AffectsMem = getRecordBool(Rec, "mayStore", false);
    bool AffectedMem = getRecordBool(Rec, "mayLoad", false);
    StringVector AffectsReg = getRegisterList("Defs", Rec);
    StringVector AffectedReg = getRegisterList("Uses", Rec);
    std::string Itinerary = getRecordItinerary(Rec);
    Instruction In(Id, Type, Operands, Uses, Defs, Size, AffectsMem,
                   AffectedMem, AffectsReg, AffectedReg, Itinerary);
    Instructions.push_back(In);
  }
  printYaml(Instructions, OS);
}

/// Printing of the instructions to the \p OS in .yaml format.
void UnisonSpecsGenEmitter::
printYaml(std::vector<Instruction> Instructions, raw_ostream &OS) {
  OS << "---\ninstruction-set:\n\n";
  std::stringstream Buffer;
  Buffer << std::setw(3) << " "
         << "- group: allInstructions"
         << "\n";
  Buffer << std::setw(5) << " "
         << "instructions:"
         << "\n\n";
  OS << Buffer.str();
  for (Instruction In : Instructions)
    In.printAll(OS);
}

/// Returns a vector of register names extraced from a \p Field attribute of the
/// given Record \p Rec . Assumes the \p Field is a list.
StringVector UnisonSpecsGenEmitter::
getRegisterList(std::string Field, Record *Rec) {
  StringVector Regs;
  for (auto Val : *(Rec->getValueAsListInit(Field)))
    Regs.push_back(escape(Val->getAsString()));
  return Regs;
}

/// Gets the Itinerary name of the given record.
std::string UnisonSpecsGenEmitter::getRecordItinerary(Record *Rec) {
  return Rec->getValueAsDef("Itinerary")->getName().str();
}

/// Gets the size of the given record.
int UnisonSpecsGenEmitter::getRecordSize(Record *Rec) {
  return Rec->getValueAsInt("Size");
}

/// Gets the boolean Value of the given \p Field in the given record \p Rec and
/// it is not set, then returns the given default Value \p def .
bool UnisonSpecsGenEmitter::
getRecordBool(Record *Rec, std::string Field, bool Def) {
  bool Unset = false;
  bool Val = Rec->getValueAsBitOrUnset(Field, Unset);
  return Unset ? Def : Val;
}

/// Gets operands of the given field from the record. Makes pairs <Type, Name>
/// where Type gives the type of the register, or immediate value, or label; and
/// Name is the identifier given to that register/value/label (like src1).
StringPairVector *UnisonSpecsGenEmitter::
parseOperands(std::string Field, Record *Rec) {
  DagInit *Dag = Rec->getValueAsDag(Field);
  StringPairVector *Ret = new StringPairVector;
  for (int I = 0, k = Dag->getNumArgs(); I < k; ++I) {
    DefInit *Def = (DefInit *)Dag->getArg(I);
    StringVector Types = flat(Def->getDef());
    for (int J = 0, K = Types.size(); J < K; ++J) {
      std::string Type = Types[J];
      std::string Name;
      if (Type == "variable_ops")
        Name = "variable";
      else {
        std::string ArgName(Dag->getArgName(I)->getValue());
        Name = Types.size() == 1 ? ArgName : (ArgName + std::to_string(J + 1));
      }
      Ret->push_back(StringPair(Type, escape(Name)));
    }
  }
  return Ret;
}

/// Extracts all suboperands of an operand, if such exist, and returns their
/// names in a list. If they do not, just returns the name of the operand as a
/// list of one element.
StringVector UnisonSpecsGenEmitter::flat(Record *Rec) {
  StringVector Ret;
  RecordVal *Field = Rec->getValue("MIOperandInfo");
  if (Field == nullptr) {
    Ret.push_back(Rec->getNameInitAsString());
    return Ret;
  }
  DagInit *Dag = (DagInit *)Field->getValue();
  if (Dag->getNumArgs() == 0) {
    Ret.push_back(Rec->getNameInitAsString());
    return Ret;
  }
  for (auto AI = Dag->arg_begin(), AE = Dag->arg_end(); AI != AE; ++AI) {
    StringVector Subs = flat(((DefInit *)*AI)->getDef());
    Ret.insert(Ret.end(), Subs.begin(), Subs.end());
  }
  return Ret;
}

/// Returns only the names found in the given list of <Type, Name>.
StringVector UnisonSpecsGenEmitter::getNames(StringPairVector *List) {
  StringVector Names;
  for (StringPair Pair : *List)
    Names.push_back(Pair.second);
  return Names;
}

/// Applies the constraints given by \p Cons as substitutions on \p Outs .
void UnisonSpecsGenEmitter::
executeConstraints(StringPairVector *Outs, std::string Cons) {
  if (Cons.empty())
    return;
  for (std::string Con : split(Cons, ',')) {
    std::string Con0 = trim(Con);
    if (Con0.find("@earlyclobber") == 0)
      continue;
    StringVector List = split(Con0, '=');
    assert(List.size() == 2 &&
           "A constraint should involve exactly two operands");
    std::string First = escape(trim(List[0]).substr(1));
    std::string Second = escape(trim(List[1]).substr(1));
    for (auto &Out : *Outs)
      if (Out.second == First)
        Out.second = Second;
      else if (Out.second == Second)
        Out.second = First;
  }
}

/// Constructs a list of full list of operands, from given input operands and
/// output operands.
OperandVector UnisonSpecsGenEmitter::
getOperands(StringPairVector *Outs, StringPairVector *Ins,
            RecordKeeper &Records) {
  OperandVector Operands;
  getOperandsFromVector(Outs, Ins, &Operands, true, Records);
  getOperandsFromVector(Ins, Outs, &Operands, false, Records);
  return Operands;
}

/// Adds operands from the \p vec list of operands to the \p operand list.
void UnisonSpecsGenEmitter::
getOperandsFromVector(StringPairVector *Vec, StringPairVector *Help,
                      OperandVector *Operands, bool Defs,
                      RecordKeeper &Records) {
  for (StringPair Pair : *Vec) {
    unison::Operand *Op = new unison::Operand;
    Op->Name = Pair.second;

    bool Flag = false;
    for (auto Op1 : *Operands)
      if (Op1.Name == Op->Name) {
        Flag = true;
        delete Op;
        break;
      }
    if (Flag)
      continue;

    std::string UseDefF = Defs ? "def" : "use";
    if (std::find(Help->begin(), Help->end(), Pair) != Help->end())
      UseDefF = Defs ? "use" + UseDefF : UseDefF + "def";
    Op->UseDef = UseDefF;
    Op->RegType = Pair.first;

    Record *Def = Records.getDef(Op->RegType);

    if (isRegister(Def))
      Op->Type = unison::Operand::Register;
    else if (isLabel(Def))
      Op->Type = unison::Operand::Label;
    else
      Op->Type = unison::Operand::Bound;
    Operands->push_back(*Op);
  }
}

bool UnisonSpecsGenEmitter::isLabel(Record *Rec) {
  // Gets ValueType.
  RecordVal *Val = Rec->getValue("Type");
  if (Val == nullptr)
    return false;
  DefInit *Def = (DefInit *)Val->getValue();
  // Supposedly the mark for the label.
  return Def->getAsString() == "OtherVT";
}

bool UnisonSpecsGenEmitter::isRegister(Record *Rec) {
  if (Rec == nullptr)
    return false;
  for (auto Super : Rec->getSuperClasses())
    // Class names that suggest that the object is a register.
    for (auto Name :
         {"RegisterClass", "Register", "RegisterOperand", "RegisterTuples"})
      if (Super.first->getName() == Name)
        return true;
  return false;
}

/// Returns the string the describes the type of the record as "call", "linear"
/// or "branch".
std::string UnisonSpecsGenEmitter::getRecordType(Record *Rec) {
  if (getRecordBool(Rec, "isCall", false))
    return "call";
  if (getRecordBool(Rec, "isBranch", false) ||
      getRecordBool(Rec, "isReturn", false))
    return "branch";
  return "linear";
}

std::string UnisonSpecsGenEmitter::getRecordId(Record *Rec) {
  return Rec->getName().str();
}

/// Cheks whether all attributes of the given record \p Rec are present for the
/// record to be analyzed as a instruction.
bool UnisonSpecsGenEmitter::allNeededFieldsExist(Record *Rec) {
  for (std::string Field :
       {"isCall", "isBranch", "Constraints", "OutOperandList", "InOperandList",
        "Size", "mayLoad", "mayStore", "Itinerary", "isReturn", "Uses", "Defs"})
    if (!fieldExists(Rec, Field))
      return false;
  return true;
}

/// Checks whether a given attribute \p Field exists in the given record \p Rec.
bool UnisonSpecsGenEmitter::fieldExists(Record *Rec, std::string Field) {
  return Rec->getValue(Field) != nullptr;
}

/// Splits the string \p Str with delimiter \p Del and returns a vector of
/// strings.
StringVector UnisonSpecsGenEmitter::split(std::string Str, char Del) {
  std::stringstream Buffer(Str);
  std::string Element;
  StringVector Ret;
  while (getline(Buffer, Element, Del))
    Ret.push_back(Element);
  return Ret;
}

/// Trims the given string and returns the result.
std::string UnisonSpecsGenEmitter::trim(std::string Str) {
  std::string WhiteSpaceChars(" \n\t\r");
  Str.erase(0, Str.find_first_not_of(WhiteSpaceChars));
  Str.erase(Str.find_last_not_of(WhiteSpaceChars) + 1);
  return Str;
}

/// Escapes YAML reserved words in the given string.
std::string UnisonSpecsGenEmitter::escape(std::string Name) {
  std::string Lname(Name);
  std::transform(Lname.begin(), Lname.end(), Lname.begin(), ::tolower);
  std::set<std::string> Reserved(
      {"true", "false", "n", "y", "yes", "no", "on", "off"});
  if (Reserved.count(Lname))
    return Name + "'";
  return Name;
}

static TableGen::Emitter::OptClass<UnisonSpecsGenEmitter>
    X("gen-unison-specsgen", "Generate Unison SpecsGen's processor description");
