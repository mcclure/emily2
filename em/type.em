# Types

profile experimental

from project.util import *
from project.core import *

export Type = inherit Object
	toString = "[MYSTERY TYPE]"

# TODO: Not like this
let typeDebug = function(to, frm, why)
	print
		why + " type of ", to.loc.toString
		if (to)
			" (" + to.type.toString + ")"
		else
			""
		" to ", frm.loc.toString
		if (frm)
			" (" + frm.type.toString + ")"
		else
			""
		ln

export TypedNode = inherit Node
	method unify = function(to)
		let frm = this.resolve
		to = to.resolve

		if (frm.type && to.type)
			#typeDebug frm to "Unifying"
			if (!frm.type.compatible (to.type))
				fail
					nullJoin array
						"Type error between ", frm.type, " (", frm.loc
						") and ", to.type, " (", to.loc, ")"
		elif (frm.type)
			#typeDebug to frm "Setting"
			to.type = frm.type
		elif (to.type)
			#typeDebug frm to "Setting"
			frm.type = to.type
		else
			#typeDebug frm to "Forwarding"
			frm.type = new ReferType(to)

	method resolve = do
		with (this.type) match
			ReferType to = to
			_ = this

	method toString = "[Node " + this.loc.toString + " type " + (if (this.type) (this.type.toString) else ("{Unresolved}")) + "]"

export ReferType = inherit Type
	field to = null

	method toString = if (this.to.type)
		this.to.type.toString
	else
		"{Unresolved type at " + this.to.loc.toString + "}"

export ResolvedType = inherit Type
	method compatible = function (type) (this.compatibleTest type || type.compatibleTest this)
	method compatibleTest = function (type) (type == this)
	method unifyArgResult = function (arg, result) (result.unify standardUnknowableVal)

export UnknowableType = inherit ResolvedType
	method compatibleTest = function (type) (true) # FIXME
	toString = "{Any}"

export InvalidType = inherit ResolvedType
	method compatibleTest = function (type) (false)
	toString = "{INTERNAL ERROR}"

export UnitType = inherit ResolvedType
	toString = "{Unit}"

export BooleanType = inherit ResolvedType
	toString = "{Boolean}"

export NumberType = inherit ResolvedType
	toString = "{Float}"

export AtomType = inherit ResolvedType
	toString = "{Atom}"

export StringType = inherit ResolvedType
	toString = "{String}"

export FunctionType = inherit ResolvedType
	field arg = null   # These are both TypedNodes
	field result = null
	method toString = nullJoin array
		"{Function ", this.arg.type, " -> ", this.result.type, "}"
	method compatibleTest = function (type)
		is FunctionType type && this.arg.resolve.type.compatibleTest (type.arg.resolve.type) && this.result.resolve.type.compatibleTest (type.result.resolve.type)
	method unifyArgResult = function(arg, result) # TODO: Check correctness of typedNode?
		arg.unify(this.arg)
		result.unify(this.result)

export Val = inherit TypedNode
	field type = null

export KnownTypeVal = inherit Val # Use for typed literals with no other content

let standardUnknowableVal = new KnownTypeVal(null, UnknowableType)

export functionType = foldr InvalidType function(prev, next)
	new FunctionType
		new KnownTypeVal(type=prev)
		new KnownTypeVal(type=next)

export functionTypeVal = function(a)
	new KnownTypeVal
		type = functionType a