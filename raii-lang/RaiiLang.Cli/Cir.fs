module RaiiLang.Cir

open RaiiLang.Helpers

type CUni =
  // `*x`
  | CDerefUni

  // `&x`
  | CRefUni

type CBin =
  | CEqBin
  | CAddBin
  | CAssignBin

type CTy =
  | CVoidTy
  | CCharTy
  | CIntTy
  | CPtrTy
    of CTy
  | CFunTy
    of CTy list * CTy

[<Struct>]
type CParam =
  | CParam
    of name:string * CTy

type CTerm =
  | CInt
    of string

  | CStr
    of StrSegment list

  | CName
    of string

  | CUni
    of CUni * CTerm

  | CCall
    of CTerm * CTerm list

  | CBin
    of CBin * CTerm * CTerm

type CStmt =
  | CTermStmt
    of CTerm

  | CLocalStmt
    of string * CTy * CTerm option

  | CIfStmt
    of CTerm * CStmt list * CStmt list

  | CGotoStmt
    of string

  | CReturn
    of CTerm option

  | CLabelStmt
    of string

type CDecl =
  | CFnDecl
    of funName:string
      * CParam list
      * CTy
      * CStmt list

  | CExternFnDecl
    of funName:string
      * CParam list
      * CTy
