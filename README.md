# Multi-class inheritance
	Author: Tung Thanh Le
	Contact: ttungl at gmail
## Introduction
Used Scheme (Dr. Racket) to modify the interpreter for creating new functions of a language. In this work, multi-class inheritance is created. New instance generated is inherited to all the methods from joined classes. Used Scheme language for implementation.
## In-Detail
This work extends a new capability of a `Classes` language in Scheme. It allows a class that can be created from two existing classes and owns its inherited properties (fields & methods) from two existing classes. It also allows to set/get a value of a class’s field.
## Grammar
* `Program::= {ClassDecl}* {ClassExtendMerge} * expression`
* `ClassDecl::= class Identifier 
		{field Identifier}*
		{MethodDecl}*`
* `MethodDecl::= method Identifier
		({Identifier}*(,))
		expression`
* `ClassExtendMerge::= classmerge Identifier Identifier Identifier`
* `Expression::= setfield identifier identifier identifier`
* `Expression::= getfield identifier identifier`

	Description: `classmerge` allows merging two existing classes and adding to a third class. This could be added up to `n` classes and all the methods and fields in `n` classes are inherited to the class `(n+1)^th`.

## Abstract syntax/ADT
* `setfield-exp: Identifier x Identifier x Expression -> Unspecified`
* `getfield-exp: Identifier x Identifier -> Expval`
* `Expval = Int + Bool + Proc + listof(expval) + object`
* `Denval = Ref(expval)`

## Main modifications:
1. `Data-types`:
	* `Object`: add a field-names.
	* `Method`: eliminate a super-name.
	* `Class`: replace super-name by prototype, and add the fields value.
		`a-class` datatype has the prototype, the field-names and fields value, and method-env.
2. `proto-merge function`:
	* This allows two existing classes can be merged and be added to a new class by appending `fields`, `field-names`, and merging `method-env` of both classes, and update this to the global class environment `the-class-env` by using `add-to-class-env!` with the new class merged.
3. `Find-method`: 
	* Find a method based on the method name in the class’s prototype.
4. `Construct-field-names` and `construct-fields`:
	* Construct the `fields` and `field-names` into the class’ prototypes.
5. `add-to-class-env!`:
	* Update a new class to the list of the global class environment `the-class-env`. It also checks the class is whether or not in the existing class of the list, if so, it updates the latest one.
6. `Update-class-env`:
	* Updates a new class to the list of the global class environment.

## How to use 
	class c1
		field f1
		field f2
		field f4
		method initialize() set f1 = 1
		method getf4() f4
	class c2
		field f3
		field f4
		method initialize() set f2 = 2
		method setf1(n)set f1 = n
		method setf2(n)set f2 = n
		method setf3(n)set f3 = n
		method setf4(n)set f4 = n
		method getf1() f1
		method getf2() f2
		method getf3() f3
	
	classmerge c4 = c1 & c2
	
	let o = new c4()
	t1=0
	t2=0
	t3=0
	t4=0
	in begin
	setfield c1 f1 = 5;
	send o setf1(20);
	set t2 = getfield c1 f1;
	set t1 = send o getf1();
	send o setf4(30);
	setfield c2 f4 =15;
	set t3 = getfield c2 f4;
	set t4 = send o getf4();
	list(t1,t2,t3,t4)
	end
	=> (20 5 15 30)

## Explanation of the above example
* We merge all fields and methods of class `c1` and `c2`, then add to class `c4`. Now, we create an instance `o` with all properties of class `c4` which inherits from class `c1` and `c2`.
* When `field 1` in class `c1` is set to `5`, and `f1` in instance `o` is set to `20`, and set `f4` in class `c2` to `15`, and then set `f4` in the instance `o` to `30`, we get the list of result as follows `(20 5 15 30)`. This means that the instance `o` has all properties of class `c4` which is inherited from class `c1` and `c2`.

