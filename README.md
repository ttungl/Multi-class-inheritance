# Multi-class inheritance
## Introduction
Used Scheme (Dr. Racket) to modify the interpreter for creating new functions of a language. In this work, multi-class inheritance is created. New instance generated is inherited to all the methods from joined classes. Used Scheme language for implementation.

## In-Detail
This work extends a new capability of a Classes language in Scheme language. It allows a class that can be created from two existing classes and owns its inherited properties (fields & methods) from two existing classes. It also allows to set/get a value of a class’s field.

## Grammar
`Program::= {ClassDecl}* {ClassExtendMerge} * expression`
`ClassDecl::= class Identifier
{field Identifier}*
{MethodDecl}*
MethodDecl::= method Identifier
({Identifier}*(,))
expression`
`ClassExtendMerge::= classmerge Identifier Identifier Identifier`
`Expression::= setfield identifier identifier identifier`
`Expression::= getfield identifier identifier`

	* Description: classmerge allows merging two existing classes and adding to a third class.
