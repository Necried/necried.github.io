(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2(elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}



// VIRTUAL-DOM WIDGETS


var _Markdown_toHtml = F3(function(options, factList, rawMarkdown)
{
	return _VirtualDom_custom(
		factList,
		{
			a: options,
			b: rawMarkdown
		},
		_Markdown_render,
		_Markdown_diff
	);
});



// WIDGET IMPLEMENTATION


function _Markdown_render(model)
{
	return A2(_Markdown_replace, model, _VirtualDom_doc.createElement('div'));
}


function _Markdown_diff(x, y)
{
	return x.b === y.b && x.a === y.a
		? false
		: _Markdown_replace(y);
}


var _Markdown_replace = F2(function(model, div)
{
	div.innerHTML = _Markdown_marked(model.b, _Markdown_formatOptions(model.a));
	return div;
});



// ACTUAL MARKDOWN PARSER


var _Markdown_marked = function() {
	// catch the `marked` object regardless of the outer environment.
	// (ex. a CommonJS module compatible environment.)
	// note that this depends on marked's implementation of environment detection.
	var module = {};
	var exports = module.exports = {};

	/**
	 * marked - a markdown parser
	 * Copyright (c) 2011-2014, Christopher Jeffrey. (MIT Licensed)
	 * https://github.com/chjj/marked
	 * commit cd2f6f5b7091154c5526e79b5f3bfb4d15995a51
	 */
	(function(){var block={newline:/^\n+/,code:/^( {4}[^\n]+\n*)+/,fences:noop,hr:/^( *[-*_]){3,} *(?:\n+|$)/,heading:/^ *(#{1,6}) *([^\n]+?) *#* *(?:\n+|$)/,nptable:noop,lheading:/^([^\n]+)\n *(=|-){2,} *(?:\n+|$)/,blockquote:/^( *>[^\n]+(\n(?!def)[^\n]+)*\n*)+/,list:/^( *)(bull) [\s\S]+?(?:hr|def|\n{2,}(?! )(?!\1bull )\n*|\s*$)/,html:/^ *(?:comment *(?:\n|\s*$)|closed *(?:\n{2,}|\s*$)|closing *(?:\n{2,}|\s*$))/,def:/^ *\[([^\]]+)\]: *<?([^\s>]+)>?(?: +["(]([^\n]+)[")])? *(?:\n+|$)/,table:noop,paragraph:/^((?:[^\n]+\n?(?!hr|heading|lheading|blockquote|tag|def))+)\n*/,text:/^[^\n]+/};block.bullet=/(?:[*+-]|\d+\.)/;block.item=/^( *)(bull) [^\n]*(?:\n(?!\1bull )[^\n]*)*/;block.item=replace(block.item,"gm")(/bull/g,block.bullet)();block.list=replace(block.list)(/bull/g,block.bullet)("hr","\\n+(?=\\1?(?:[-*_] *){3,}(?:\\n+|$))")("def","\\n+(?="+block.def.source+")")();block.blockquote=replace(block.blockquote)("def",block.def)();block._tag="(?!(?:"+"a|em|strong|small|s|cite|q|dfn|abbr|data|time|code"+"|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo"+"|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b";block.html=replace(block.html)("comment",/<!--[\s\S]*?-->/)("closed",/<(tag)[\s\S]+?<\/\1>/)("closing",/<tag(?:"[^"]*"|'[^']*'|[^'">])*?>/)(/tag/g,block._tag)();block.paragraph=replace(block.paragraph)("hr",block.hr)("heading",block.heading)("lheading",block.lheading)("blockquote",block.blockquote)("tag","<"+block._tag)("def",block.def)();block.normal=merge({},block);block.gfm=merge({},block.normal,{fences:/^ *(`{3,}|~{3,})[ \.]*(\S+)? *\n([\s\S]*?)\s*\1 *(?:\n+|$)/,paragraph:/^/,heading:/^ *(#{1,6}) +([^\n]+?) *#* *(?:\n+|$)/});block.gfm.paragraph=replace(block.paragraph)("(?!","(?!"+block.gfm.fences.source.replace("\\1","\\2")+"|"+block.list.source.replace("\\1","\\3")+"|")();block.tables=merge({},block.gfm,{nptable:/^ *(\S.*\|.*)\n *([-:]+ *\|[-| :]*)\n((?:.*\|.*(?:\n|$))*)\n*/,table:/^ *\|(.+)\n *\|( *[-:]+[-| :]*)\n((?: *\|.*(?:\n|$))*)\n*/});function Lexer(options){this.tokens=[];this.tokens.links={};this.options=options||marked.defaults;this.rules=block.normal;if(this.options.gfm){if(this.options.tables){this.rules=block.tables}else{this.rules=block.gfm}}}Lexer.rules=block;Lexer.lex=function(src,options){var lexer=new Lexer(options);return lexer.lex(src)};Lexer.prototype.lex=function(src){src=src.replace(/\r\n|\r/g,"\n").replace(/\t/g,"    ").replace(/\u00a0/g," ").replace(/\u2424/g,"\n");return this.token(src,true)};Lexer.prototype.token=function(src,top,bq){var src=src.replace(/^ +$/gm,""),next,loose,cap,bull,b,item,space,i,l;while(src){if(cap=this.rules.newline.exec(src)){src=src.substring(cap[0].length);if(cap[0].length>1){this.tokens.push({type:"space"})}}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);cap=cap[0].replace(/^ {4}/gm,"");this.tokens.push({type:"code",text:!this.options.pedantic?cap.replace(/\n+$/,""):cap});continue}if(cap=this.rules.fences.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"code",lang:cap[2],text:cap[3]||""});continue}if(cap=this.rules.heading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[1].length,text:cap[2]});continue}if(top&&(cap=this.rules.nptable.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].split(/ *\| */)}this.tokens.push(item);continue}if(cap=this.rules.lheading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[2]==="="?1:2,text:cap[1]});continue}if(cap=this.rules.hr.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"hr"});continue}if(cap=this.rules.blockquote.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"blockquote_start"});cap=cap[0].replace(/^ *> ?/gm,"");this.token(cap,top,true);this.tokens.push({type:"blockquote_end"});continue}if(cap=this.rules.list.exec(src)){src=src.substring(cap[0].length);bull=cap[2];this.tokens.push({type:"list_start",ordered:bull.length>1});cap=cap[0].match(this.rules.item);next=false;l=cap.length;i=0;for(;i<l;i++){item=cap[i];space=item.length;item=item.replace(/^ *([*+-]|\d+\.) +/,"");if(~item.indexOf("\n ")){space-=item.length;item=!this.options.pedantic?item.replace(new RegExp("^ {1,"+space+"}","gm"),""):item.replace(/^ {1,4}/gm,"")}if(this.options.smartLists&&i!==l-1){b=block.bullet.exec(cap[i+1])[0];if(bull!==b&&!(bull.length>1&&b.length>1)){src=cap.slice(i+1).join("\n")+src;i=l-1}}loose=next||/\n\n(?!\s*$)/.test(item);if(i!==l-1){next=item.charAt(item.length-1)==="\n";if(!loose)loose=next}this.tokens.push({type:loose?"loose_item_start":"list_item_start"});this.token(item,false,bq);this.tokens.push({type:"list_item_end"})}this.tokens.push({type:"list_end"});continue}if(cap=this.rules.html.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:this.options.sanitize?"paragraph":"html",pre:!this.options.sanitizer&&(cap[1]==="pre"||cap[1]==="script"||cap[1]==="style"),text:cap[0]});continue}if(!bq&&top&&(cap=this.rules.def.exec(src))){src=src.substring(cap[0].length);this.tokens.links[cap[1].toLowerCase()]={href:cap[2],title:cap[3]};continue}if(top&&(cap=this.rules.table.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/(?: *\| *)?\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].replace(/^ *\| *| *\| *$/g,"").split(/ *\| */)}this.tokens.push(item);continue}if(top&&(cap=this.rules.paragraph.exec(src))){src=src.substring(cap[0].length);this.tokens.push({type:"paragraph",text:cap[1].charAt(cap[1].length-1)==="\n"?cap[1].slice(0,-1):cap[1]});continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"text",text:cap[0]});continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return this.tokens};var inline={escape:/^\\([\\`*{}\[\]()#+\-.!_>])/,autolink:/^<([^ >]+(@|:\/)[^ >]+)>/,url:noop,tag:/^<!--[\s\S]*?-->|^<\/?\w+(?:"[^"]*"|'[^']*'|[^'">])*?>/,link:/^!?\[(inside)\]\(href\)/,reflink:/^!?\[(inside)\]\s*\[([^\]]*)\]/,nolink:/^!?\[((?:\[[^\]]*\]|[^\[\]])*)\]/,strong:/^_\_([\s\S]+?)_\_(?!_)|^\*\*([\s\S]+?)\*\*(?!\*)/,em:/^\b_((?:[^_]|_\_)+?)_\b|^\*((?:\*\*|[\s\S])+?)\*(?!\*)/,code:/^(`+)\s*([\s\S]*?[^`])\s*\1(?!`)/,br:/^ {2,}\n(?!\s*$)/,del:noop,text:/^[\s\S]+?(?=[\\<!\[_*`]| {2,}\n|$)/};inline._inside=/(?:\[[^\]]*\]|[^\[\]]|\](?=[^\[]*\]))*/;inline._href=/\s*<?([\s\S]*?)>?(?:\s+['"]([\s\S]*?)['"])?\s*/;inline.link=replace(inline.link)("inside",inline._inside)("href",inline._href)();inline.reflink=replace(inline.reflink)("inside",inline._inside)();inline.normal=merge({},inline);inline.pedantic=merge({},inline.normal,{strong:/^_\_(?=\S)([\s\S]*?\S)_\_(?!_)|^\*\*(?=\S)([\s\S]*?\S)\*\*(?!\*)/,em:/^_(?=\S)([\s\S]*?\S)_(?!_)|^\*(?=\S)([\s\S]*?\S)\*(?!\*)/});inline.gfm=merge({},inline.normal,{escape:replace(inline.escape)("])","~|])")(),url:/^(https?:\/\/[^\s<]+[^<.,:;"')\]\s])/,del:/^~~(?=\S)([\s\S]*?\S)~~/,text:replace(inline.text)("]|","~]|")("|","|https?://|")()});inline.breaks=merge({},inline.gfm,{br:replace(inline.br)("{2,}","*")(),text:replace(inline.gfm.text)("{2,}","*")()});function InlineLexer(links,options){this.options=options||marked.defaults;this.links=links;this.rules=inline.normal;this.renderer=this.options.renderer||new Renderer;this.renderer.options=this.options;if(!this.links){throw new Error("Tokens array requires a `links` property.")}if(this.options.gfm){if(this.options.breaks){this.rules=inline.breaks}else{this.rules=inline.gfm}}else if(this.options.pedantic){this.rules=inline.pedantic}}InlineLexer.rules=inline;InlineLexer.output=function(src,links,options){var inline=new InlineLexer(links,options);return inline.output(src)};InlineLexer.prototype.output=function(src){var out="",link,text,href,cap;while(src){if(cap=this.rules.escape.exec(src)){src=src.substring(cap[0].length);out+=cap[1];continue}if(cap=this.rules.autolink.exec(src)){src=src.substring(cap[0].length);if(cap[2]==="@"){text=cap[1].charAt(6)===":"?this.mangle(cap[1].substring(7)):this.mangle(cap[1]);href=this.mangle("mailto:")+text}else{text=escape(cap[1]);href=text}out+=this.renderer.link(href,null,text);continue}if(!this.inLink&&(cap=this.rules.url.exec(src))){src=src.substring(cap[0].length);text=escape(cap[1]);href=text;out+=this.renderer.link(href,null,text);continue}if(cap=this.rules.tag.exec(src)){if(!this.inLink&&/^<a /i.test(cap[0])){this.inLink=true}else if(this.inLink&&/^<\/a>/i.test(cap[0])){this.inLink=false}src=src.substring(cap[0].length);out+=this.options.sanitize?this.options.sanitizer?this.options.sanitizer(cap[0]):escape(cap[0]):cap[0];continue}if(cap=this.rules.link.exec(src)){src=src.substring(cap[0].length);this.inLink=true;out+=this.outputLink(cap,{href:cap[2],title:cap[3]});this.inLink=false;continue}if((cap=this.rules.reflink.exec(src))||(cap=this.rules.nolink.exec(src))){src=src.substring(cap[0].length);link=(cap[2]||cap[1]).replace(/\s+/g," ");link=this.links[link.toLowerCase()];if(!link||!link.href){out+=cap[0].charAt(0);src=cap[0].substring(1)+src;continue}this.inLink=true;out+=this.outputLink(cap,link);this.inLink=false;continue}if(cap=this.rules.strong.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.strong(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.em.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.em(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.codespan(escape(cap[2],true));continue}if(cap=this.rules.br.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.br();continue}if(cap=this.rules.del.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.del(this.output(cap[1]));continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.text(escape(this.smartypants(cap[0])));continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return out};InlineLexer.prototype.outputLink=function(cap,link){var href=escape(link.href),title=link.title?escape(link.title):null;return cap[0].charAt(0)!=="!"?this.renderer.link(href,title,this.output(cap[1])):this.renderer.image(href,title,escape(cap[1]))};InlineLexer.prototype.smartypants=function(text){if(!this.options.smartypants)return text;return text.replace(/---/g,"—").replace(/--/g,"–").replace(/(^|[-\u2014\/(\[{"\s])'/g,"$1‘").replace(/'/g,"’").replace(/(^|[-\u2014\/(\[{\u2018\s])"/g,"$1“").replace(/"/g,"”").replace(/\.{3}/g,"…")};InlineLexer.prototype.mangle=function(text){if(!this.options.mangle)return text;var out="",l=text.length,i=0,ch;for(;i<l;i++){ch=text.charCodeAt(i);if(Math.random()>.5){ch="x"+ch.toString(16)}out+="&#"+ch+";"}return out};function Renderer(options){this.options=options||{}}Renderer.prototype.code=function(code,lang,escaped){if(this.options.highlight){var out=this.options.highlight(code,lang);if(out!=null&&out!==code){escaped=true;code=out}}if(!lang){return"<pre><code>"+(escaped?code:escape(code,true))+"\n</code></pre>"}return'<pre><code class="'+this.options.langPrefix+escape(lang,true)+'">'+(escaped?code:escape(code,true))+"\n</code></pre>\n"};Renderer.prototype.blockquote=function(quote){return"<blockquote>\n"+quote+"</blockquote>\n"};Renderer.prototype.html=function(html){return html};Renderer.prototype.heading=function(text,level,raw){return"<h"+level+' id="'+this.options.headerPrefix+raw.toLowerCase().replace(/[^\w]+/g,"-")+'">'+text+"</h"+level+">\n"};Renderer.prototype.hr=function(){return this.options.xhtml?"<hr/>\n":"<hr>\n"};Renderer.prototype.list=function(body,ordered){var type=ordered?"ol":"ul";return"<"+type+">\n"+body+"</"+type+">\n"};Renderer.prototype.listitem=function(text){return"<li>"+text+"</li>\n"};Renderer.prototype.paragraph=function(text){return"<p>"+text+"</p>\n"};Renderer.prototype.table=function(header,body){return"<table>\n"+"<thead>\n"+header+"</thead>\n"+"<tbody>\n"+body+"</tbody>\n"+"</table>\n"};Renderer.prototype.tablerow=function(content){return"<tr>\n"+content+"</tr>\n"};Renderer.prototype.tablecell=function(content,flags){var type=flags.header?"th":"td";var tag=flags.align?"<"+type+' style="text-align:'+flags.align+'">':"<"+type+">";return tag+content+"</"+type+">\n"};Renderer.prototype.strong=function(text){return"<strong>"+text+"</strong>"};Renderer.prototype.em=function(text){return"<em>"+text+"</em>"};Renderer.prototype.codespan=function(text){return"<code>"+text+"</code>"};Renderer.prototype.br=function(){return this.options.xhtml?"<br/>":"<br>"};Renderer.prototype.del=function(text){return"<del>"+text+"</del>"};Renderer.prototype.link=function(href,title,text){if(this.options.sanitize){try{var prot=decodeURIComponent(unescape(href)).replace(/[^\w:]/g,"").toLowerCase()}catch(e){return""}if(prot.indexOf("javascript:")===0||prot.indexOf("vbscript:")===0||prot.indexOf("data:")===0){return""}}var out='<a href="'+href+'"';if(title){out+=' title="'+title+'"'}out+=">"+text+"</a>";return out};Renderer.prototype.image=function(href,title,text){var out='<img src="'+href+'" alt="'+text+'"';if(title){out+=' title="'+title+'"'}out+=this.options.xhtml?"/>":">";return out};Renderer.prototype.text=function(text){return text};function Parser(options){this.tokens=[];this.token=null;this.options=options||marked.defaults;this.options.renderer=this.options.renderer||new Renderer;this.renderer=this.options.renderer;this.renderer.options=this.options}Parser.parse=function(src,options,renderer){var parser=new Parser(options,renderer);return parser.parse(src)};Parser.prototype.parse=function(src){this.inline=new InlineLexer(src.links,this.options,this.renderer);this.tokens=src.reverse();var out="";while(this.next()){out+=this.tok()}return out};Parser.prototype.next=function(){return this.token=this.tokens.pop()};Parser.prototype.peek=function(){return this.tokens[this.tokens.length-1]||0};Parser.prototype.parseText=function(){var body=this.token.text;while(this.peek().type==="text"){body+="\n"+this.next().text}return this.inline.output(body)};Parser.prototype.tok=function(){switch(this.token.type){case"space":{return""}case"hr":{return this.renderer.hr()}case"heading":{return this.renderer.heading(this.inline.output(this.token.text),this.token.depth,this.token.text)}case"code":{return this.renderer.code(this.token.text,this.token.lang,this.token.escaped)}case"table":{var header="",body="",i,row,cell,flags,j;cell="";for(i=0;i<this.token.header.length;i++){flags={header:true,align:this.token.align[i]};cell+=this.renderer.tablecell(this.inline.output(this.token.header[i]),{header:true,align:this.token.align[i]})}header+=this.renderer.tablerow(cell);for(i=0;i<this.token.cells.length;i++){row=this.token.cells[i];cell="";for(j=0;j<row.length;j++){cell+=this.renderer.tablecell(this.inline.output(row[j]),{header:false,align:this.token.align[j]})}body+=this.renderer.tablerow(cell)}return this.renderer.table(header,body)}case"blockquote_start":{var body="";while(this.next().type!=="blockquote_end"){body+=this.tok()}return this.renderer.blockquote(body)}case"list_start":{var body="",ordered=this.token.ordered;while(this.next().type!=="list_end"){body+=this.tok()}return this.renderer.list(body,ordered)}case"list_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.token.type==="text"?this.parseText():this.tok()}return this.renderer.listitem(body)}case"loose_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.tok()}return this.renderer.listitem(body)}case"html":{var html=!this.token.pre&&!this.options.pedantic?this.inline.output(this.token.text):this.token.text;return this.renderer.html(html)}case"paragraph":{return this.renderer.paragraph(this.inline.output(this.token.text))}case"text":{return this.renderer.paragraph(this.parseText())}}};function escape(html,encode){return html.replace(!encode?/&(?!#?\w+;)/g:/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")}function unescape(html){return html.replace(/&(#(?:\d+)|(?:#x[0-9A-Fa-f]+)|(?:\w+));?/g,function(_,n){n=n.toLowerCase();if(n==="colon")return":";if(n.charAt(0)==="#"){return n.charAt(1)==="x"?String.fromCharCode(parseInt(n.substring(2),16)):String.fromCharCode(+n.substring(1))}return""})}function replace(regex,opt){regex=regex.source;opt=opt||"";return function self(name,val){if(!name)return new RegExp(regex,opt);val=val.source||val;val=val.replace(/(^|[^\[])\^/g,"$1");regex=regex.replace(name,val);return self}}function noop(){}noop.exec=noop;function merge(obj){var i=1,target,key;for(;i<arguments.length;i++){target=arguments[i];for(key in target){if(Object.prototype.hasOwnProperty.call(target,key)){obj[key]=target[key]}}}return obj}function marked(src,opt,callback){if(callback||typeof opt==="function"){if(!callback){callback=opt;opt=null}opt=merge({},marked.defaults,opt||{});var highlight=opt.highlight,tokens,pending,i=0;try{tokens=Lexer.lex(src,opt)}catch(e){return callback(e)}pending=tokens.length;var done=function(err){if(err){opt.highlight=highlight;return callback(err)}var out;try{out=Parser.parse(tokens,opt)}catch(e){err=e}opt.highlight=highlight;return err?callback(err):callback(null,out)};if(!highlight||highlight.length<3){return done()}delete opt.highlight;if(!pending)return done();for(;i<tokens.length;i++){(function(token){if(token.type!=="code"){return--pending||done()}return highlight(token.text,token.lang,function(err,code){if(err)return done(err);if(code==null||code===token.text){return--pending||done()}token.text=code;token.escaped=true;--pending||done()})})(tokens[i])}return}try{if(opt)opt=merge({},marked.defaults,opt);return Parser.parse(Lexer.lex(src,opt),opt)}catch(e){e.message+="\nPlease report this to https://github.com/chjj/marked.";if((opt||marked.defaults).silent){return"<p>An error occured:</p><pre>"+escape(e.message+"",true)+"</pre>"}throw e}}marked.options=marked.setOptions=function(opt){merge(marked.defaults,opt);return marked};marked.defaults={gfm:true,tables:true,breaks:false,pedantic:false,sanitize:false,sanitizer:null,mangle:true,smartLists:false,silent:false,highlight:null,langPrefix:"lang-",smartypants:false,headerPrefix:"",renderer:new Renderer,xhtml:false};marked.Parser=Parser;marked.parser=Parser.parse;marked.Renderer=Renderer;marked.Lexer=Lexer;marked.lexer=Lexer.lex;marked.InlineLexer=InlineLexer;marked.inlineLexer=InlineLexer.output;marked.parse=marked;if(typeof module!=="undefined"&&typeof exports==="object"){module.exports=marked}else if(typeof define==="function"&&define.amd){define(function(){return marked})}else{this.marked=marked}}).call(function(){return this||(typeof window!=="undefined"?window:global)}());

	return module.exports;
}();


// FORMAT OPTIONS FOR MARKED IMPLEMENTATION

function _Markdown_formatOptions(options)
{
	function toHighlight(code, lang)
	{
		if (!lang && elm$core$Maybe$isJust(options.defaultHighlighting))
		{
			lang = options.defaultHighlighting.a;
		}

		if (typeof hljs !== 'undefined' && lang && hljs.listLanguages().indexOf(lang) >= 0)
		{
			return hljs.highlight(lang, code, true).value;
		}

		return code;
	}

	var gfm = options.githubFlavored.a;

	return {
		highlight: toHighlight,
		gfm: gfm,
		tables: gfm && gfm.tables,
		breaks: gfm && gfm.breaks,
		sanitize: options.sanitize,
		smartypants: options.smartypants
	};
}
var author$project$Main$LinkClicked = function (a) {
	return {$: 'LinkClicked', a: a};
};
var author$project$Main$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var author$project$Main$NavbarMsg = function (a) {
	return {$: 'NavbarMsg', a: a};
};
var author$project$Routes$About = {$: 'About'};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$List$cons = _List_cons;
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$Active = {$: 'Active'};
var rundis$elm_bootstrap$Bootstrap$Carousel$defaultStateOptions = {
	cycling: rundis$elm_bootstrap$Bootstrap$Carousel$Active,
	interval: elm$core$Maybe$Just(5000),
	keyboard: true,
	pauseOnHover: true,
	startIndex: 0,
	wrap: true
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$IgnoreHover = {$: 'IgnoreHover'};
var rundis$elm_bootstrap$Bootstrap$Carousel$NotAnimating = {$: 'NotAnimating'};
var rundis$elm_bootstrap$Bootstrap$Carousel$NotHovered = {$: 'NotHovered'};
var rundis$elm_bootstrap$Bootstrap$Carousel$State = F2(
	function (a, b) {
		return {$: 'State', a: a, b: b};
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$initialStateWithOptions = function (options) {
	return A2(
		rundis$elm_bootstrap$Bootstrap$Carousel$State,
		rundis$elm_bootstrap$Bootstrap$Carousel$NotAnimating,
		{
			currentIndex: options.startIndex,
			cycling: options.cycling,
			hovering: options.pauseOnHover ? rundis$elm_bootstrap$Bootstrap$Carousel$NotHovered : rundis$elm_bootstrap$Bootstrap$Carousel$IgnoreHover,
			interval: A2(elm$core$Maybe$withDefault, 0, options.interval),
			keyboard: options.keyboard,
			size: 2,
			wrap: options.wrap
		});
};
var rundis$elm_bootstrap$Bootstrap$Carousel$initialState = rundis$elm_bootstrap$Bootstrap$Carousel$initialStateWithOptions(rundis$elm_bootstrap$Bootstrap$Carousel$defaultStateOptions);
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var rundis$elm_bootstrap$Bootstrap$Navbar$Hidden = {$: 'Hidden'};
var rundis$elm_bootstrap$Bootstrap$Navbar$State = function (a) {
	return {$: 'State', a: a};
};
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Basics$False = {$: 'False'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Dom$getViewport = _Browser_withWindow(_Browser_getViewport);
var rundis$elm_bootstrap$Bootstrap$Navbar$mapState = F2(
	function (mapper, _n0) {
		var state = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Navbar$State(
			mapper(state));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$initWindowSize = F2(
	function (toMsg, state) {
		return A2(
			elm$core$Task$perform,
			function (vp) {
				return toMsg(
					A2(
						rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
						function (s) {
							return _Utils_update(
								s,
								{
									windowWidth: elm$core$Maybe$Just(vp.viewport.width)
								});
						},
						state));
			},
			elm$browser$Browser$Dom$getViewport);
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$initialState = function (toMsg) {
	var state = rundis$elm_bootstrap$Bootstrap$Navbar$State(
		{dropdowns: elm$core$Dict$empty, height: elm$core$Maybe$Nothing, visibility: rundis$elm_bootstrap$Bootstrap$Navbar$Hidden, windowWidth: elm$core$Maybe$Nothing});
	return _Utils_Tuple2(
		state,
		A2(rundis$elm_bootstrap$Bootstrap$Navbar$initWindowSize, toMsg, state));
};
var rundis$elm_bootstrap$Bootstrap$Tab$Showing = {$: 'Showing'};
var rundis$elm_bootstrap$Bootstrap$Tab$State = function (a) {
	return {$: 'State', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Tab$initialState = rundis$elm_bootstrap$Bootstrap$Tab$State(
	{activeTab: elm$core$Maybe$Nothing, visibility: rundis$elm_bootstrap$Bootstrap$Tab$Showing});
var author$project$Main$init = F3(
	function (_n0, url, key) {
		var _n1 = rundis$elm_bootstrap$Bootstrap$Navbar$initialState(author$project$Main$NavbarMsg);
		var navbarState = _n1.a;
		var navbarCmd = _n1.b;
		var initModel = {
			aboutState: {languageSelection: 'Haskell'},
			carouselState: rundis$elm_bootstrap$Bootstrap$Carousel$initialState,
			key: key,
			navbarState: navbarState,
			page: author$project$Routes$About,
			tabState: rundis$elm_bootstrap$Bootstrap$Tab$initialState
		};
		return _Utils_Tuple2(initModel, navbarCmd);
	});
var author$project$Main$CarouselMsg = function (a) {
	return {$: 'CarouselMsg', a: a};
};
var author$project$Main$TabMsg = function (a) {
	return {$: 'TabMsg', a: a};
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var elm$browser$Browser$AnimationManager$init = elm$core$Task$succeed(
	A3(elm$browser$Browser$AnimationManager$State, _List_Nil, elm$core$Maybe$Nothing, 0));
var elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _n0) {
		var request = _n0.request;
		var oldTime = _n0.oldTime;
		var _n1 = _Utils_Tuple2(request, subs);
		if (_n1.a.$ === 'Nothing') {
			if (!_n1.b.b) {
				var _n2 = _n1.a;
				return elm$browser$Browser$AnimationManager$init;
			} else {
				var _n4 = _n1.a;
				return A2(
					elm$core$Task$andThen,
					function (pid) {
						return A2(
							elm$core$Task$andThen,
							function (time) {
								return elm$core$Task$succeed(
									A3(
										elm$browser$Browser$AnimationManager$State,
										subs,
										elm$core$Maybe$Just(pid),
										time));
							},
							elm$browser$Browser$AnimationManager$now);
					},
					elm$core$Process$spawn(
						A2(
							elm$core$Task$andThen,
							elm$core$Platform$sendToSelf(router),
							elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_n1.b.b) {
				var pid = _n1.a.a;
				return A2(
					elm$core$Task$andThen,
					function (_n3) {
						return elm$browser$Browser$AnimationManager$init;
					},
					elm$core$Process$kill(pid));
			} else {
				return elm$core$Task$succeed(
					A3(elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var elm$time$Time$millisToPosix = elm$time$Time$Posix;
var elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _n0) {
		var subs = _n0.subs;
		var oldTime = _n0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(
						elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			elm$core$Task$andThen,
			function (pid) {
				return A2(
					elm$core$Task$andThen,
					function (_n1) {
						return elm$core$Task$succeed(
							A3(
								elm$browser$Browser$AnimationManager$State,
								subs,
								elm$core$Maybe$Just(pid),
								newTime));
					},
					elm$core$Task$sequence(
						A2(elm$core$List$map, send, subs)));
			},
			elm$core$Process$spawn(
				A2(
					elm$core$Task$andThen,
					elm$core$Platform$sendToSelf(router),
					elm$browser$Browser$AnimationManager$rAF)));
	});
var elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Time(
				A2(elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return elm$browser$Browser$AnimationManager$Delta(
				A2(elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager(elm$browser$Browser$AnimationManager$init, elm$browser$Browser$AnimationManager$onEffects, elm$browser$Browser$AnimationManager$onSelfMsg, 0, elm$browser$Browser$AnimationManager$subMap);
var elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return elm$browser$Browser$AnimationManager$subscription(
		elm$browser$Browser$AnimationManager$Time(tagger));
};
var elm$browser$Browser$Events$onAnimationFrame = elm$browser$Browser$AnimationManager$onAnimationFrame;
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$Basics$not = _Basics_not;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var elm$time$Time$init = elm$core$Task$succeed(
	A2(elm$time$Time$State, elm$core$Dict$empty, elm$core$Dict$empty));
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$time$Time$addMySub = F2(
	function (_n0, state) {
		var interval = _n0.a;
		var tagger = _n0.b;
		var _n1 = A2(elm$core$Dict$get, interval, state);
		if (_n1.$ === 'Nothing') {
			return A3(
				elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _n1.a;
			return A3(
				elm$core$Dict$insert,
				interval,
				A2(elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var elm$time$Time$customZone = elm$time$Time$Zone;
var elm$time$Time$setInterval = _Time_setInterval;
var elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = elm$core$Process$spawn(
				A2(
					elm$time$Time$setInterval,
					interval,
					A2(elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					elm$time$Time$spawnHelp,
					router,
					rest,
					A3(elm$core$Dict$insert, interval, id, processes));
			};
			return A2(elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var elm$time$Time$onEffects = F3(
	function (router, subs, _n0) {
		var processes = _n0.processes;
		var rightStep = F3(
			function (_n6, id, _n7) {
				var spawns = _n7.a;
				var existing = _n7.b;
				var kills = _n7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						elm$core$Task$andThen,
						function (_n5) {
							return kills;
						},
						elm$core$Process$kill(id)));
			});
		var newTaggers = A3(elm$core$List$foldl, elm$time$Time$addMySub, elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _n4) {
				var spawns = _n4.a;
				var existing = _n4.b;
				var kills = _n4.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _n3) {
				var spawns = _n3.a;
				var existing = _n3.b;
				var kills = _n3.c;
				return _Utils_Tuple3(
					spawns,
					A3(elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _n1 = A6(
			elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				elm$core$Dict$empty,
				elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _n1.a;
		var existingDict = _n1.b;
		var killTask = _n1.c;
		return A2(
			elm$core$Task$andThen,
			function (newProcesses) {
				return elm$core$Task$succeed(
					A2(elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				elm$core$Task$andThen,
				function (_n2) {
					return A3(elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var elm$time$Time$now = _Time_now(elm$time$Time$millisToPosix);
var elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _n0 = A2(elm$core$Dict$get, interval, state.taggers);
		if (_n0.$ === 'Nothing') {
			return elm$core$Task$succeed(state);
		} else {
			var taggers = _n0.a;
			var tellTaggers = function (time) {
				return elm$core$Task$sequence(
					A2(
						elm$core$List$map,
						function (tagger) {
							return A2(
								elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$succeed(state);
				},
				A2(elm$core$Task$andThen, tellTaggers, elm$time$Time$now));
		}
	});
var elm$time$Time$subMap = F2(
	function (f, _n0) {
		var interval = _n0.a;
		var tagger = _n0.b;
		return A2(
			elm$time$Time$Every,
			interval,
			A2(elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager(elm$time$Time$init, elm$time$Time$onEffects, elm$time$Time$onSelfMsg, 0, elm$time$Time$subMap);
var elm$time$Time$subscription = _Platform_leaf('Time');
var elm$time$Time$every = F2(
	function (interval, tagger) {
		return elm$time$Time$subscription(
			A2(elm$time$Time$Every, interval, tagger));
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$Hovered = {$: 'Hovered'};
var rundis$elm_bootstrap$Bootstrap$Carousel$Next = {$: 'Next'};
var rundis$elm_bootstrap$Bootstrap$Carousel$SetAnimating = {$: 'SetAnimating'};
var rundis$elm_bootstrap$Bootstrap$Carousel$StartTransition = function (a) {
	return {$: 'StartTransition', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$subscriptions = F2(
	function (model, toMsg) {
		switch (model.a.$) {
			case 'NotAnimating':
				var _n1 = model.a;
				var interval = model.b.interval;
				var cycling = model.b.cycling;
				var wrap = model.b.wrap;
				var currentIndex = model.b.currentIndex;
				var hovering = model.b.hovering;
				var size = model.b.size;
				var atEnd = (!wrap) && _Utils_eq(currentIndex, size - 1);
				return (_Utils_eq(cycling, rundis$elm_bootstrap$Bootstrap$Carousel$Active) && ((!_Utils_eq(hovering, rundis$elm_bootstrap$Bootstrap$Carousel$Hovered)) && (interval && (!atEnd)))) ? A2(
					elm$time$Time$every,
					interval,
					function (_n2) {
						return toMsg(
							rundis$elm_bootstrap$Bootstrap$Carousel$StartTransition(rundis$elm_bootstrap$Bootstrap$Carousel$Next));
					}) : elm$core$Platform$Sub$none;
			case 'Start':
				var transition = model.a.a;
				return elm$browser$Browser$Events$onAnimationFrame(
					function (_n3) {
						return toMsg(rundis$elm_bootstrap$Bootstrap$Carousel$SetAnimating);
					});
			default:
				var transition = model.a.a;
				return elm$core$Platform$Sub$none;
		}
	});
var elm$browser$Browser$Events$Window = {$: 'Window'};
var elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var elm$browser$Browser$Events$init = elm$core$Task$succeed(
	A2(elm$browser$Browser$Events$State, _List_Nil, elm$core$Dict$empty));
var elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var elm$browser$Browser$Events$spawn = F3(
	function (router, key, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						elm$core$Platform$sendToSelf,
						router,
						A2(elm$browser$Browser$Events$Event, key, event));
				}));
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _n6) {
				var deads = _n6.a;
				var lives = _n6.b;
				var news = _n6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						elm$core$List$cons,
						A3(elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_n4, pid, _n5) {
				var deads = _n5.a;
				var lives = _n5.b;
				var news = _n5.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _n2, _n3) {
				var deads = _n3.a;
				var lives = _n3.b;
				var news = _n3.c;
				return _Utils_Tuple3(
					deads,
					A3(elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2(elm$core$List$map, elm$browser$Browser$Events$addKey, subs);
		var _n0 = A6(
			elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, elm$core$Dict$empty, _List_Nil));
		var deadPids = _n0.a;
		var livePids = _n0.b;
		var makeNewPids = _n0.c;
		return A2(
			elm$core$Task$andThen,
			function (pids) {
				return elm$core$Task$succeed(
					A2(
						elm$browser$Browser$Events$State,
						newSubs,
						A2(
							elm$core$Dict$union,
							livePids,
							elm$core$Dict$fromList(pids))));
			},
			A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$sequence(makeNewPids);
				},
				elm$core$Task$sequence(
					A2(elm$core$List$map, elm$core$Process$kill, deadPids))));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _n0, state) {
		var key = _n0.key;
		var event = _n0.event;
		var toMessage = function (_n2) {
			var subKey = _n2.a;
			var _n3 = _n2.b;
			var node = _n3.a;
			var name = _n3.b;
			var decoder = _n3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : elm$core$Maybe$Nothing;
		};
		var messages = A2(elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Platform$sendToApp(router),
					messages)));
	});
var elm$browser$Browser$Events$subMap = F2(
	function (func, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var decoder = _n0.c;
		return A3(
			elm$browser$Browser$Events$MySub,
			node,
			name,
			A2(elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager(elm$browser$Browser$Events$init, elm$browser$Browser$Events$onEffects, elm$browser$Browser$Events$onSelfMsg, 0, elm$browser$Browser$Events$subMap);
var elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return elm$browser$Browser$Events$subscription(
			A3(elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		elm$browser$Browser$Events$on,
		elm$browser$Browser$Events$Window,
		'resize',
		A2(
			elm$json$Json$Decode$field,
			'target',
			A3(
				elm$json$Json$Decode$map2,
				func,
				A2(elm$json$Json$Decode$field, 'innerWidth', elm$json$Json$Decode$int),
				A2(elm$json$Json$Decode$field, 'innerHeight', elm$json$Json$Decode$int))));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingDown = {$: 'AnimatingDown'};
var rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingUp = {$: 'AnimatingUp'};
var elm$browser$Browser$Events$Document = {$: 'Document'};
var elm$browser$Browser$Events$onClick = A2(elm$browser$Browser$Events$on, elm$browser$Browser$Events$Document, 'click');
var elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2(elm$core$Dict$map, func, left),
				A2(elm$core$Dict$map, func, right));
		}
	});
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$Closed = {$: 'Closed'};
var rundis$elm_bootstrap$Bootstrap$Navbar$ListenClicks = {$: 'ListenClicks'};
var rundis$elm_bootstrap$Bootstrap$Navbar$Open = {$: 'Open'};
var rundis$elm_bootstrap$Bootstrap$Navbar$dropdownSubscriptions = F2(
	function (state, toMsg) {
		var dropdowns = state.a.dropdowns;
		var updDropdowns = A2(
			elm$core$Dict$map,
			F2(
				function (_n2, status) {
					switch (status.$) {
						case 'Open':
							return rundis$elm_bootstrap$Bootstrap$Navbar$ListenClicks;
						case 'ListenClicks':
							return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
						default:
							return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
					}
				}),
			dropdowns);
		var updState = A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
			function (s) {
				return _Utils_update(
					s,
					{dropdowns: updDropdowns});
			},
			state);
		var needsSub = function (s) {
			return A2(
				elm$core$List$any,
				function (_n1) {
					var status = _n1.b;
					return _Utils_eq(status, s);
				},
				elm$core$Dict$toList(dropdowns));
		};
		return elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					needsSub(rundis$elm_bootstrap$Bootstrap$Navbar$Open) ? elm$browser$Browser$Events$onAnimationFrame(
					function (_n0) {
						return toMsg(updState);
					}) : elm$core$Platform$Sub$none,
					needsSub(rundis$elm_bootstrap$Bootstrap$Navbar$ListenClicks) ? elm$browser$Browser$Events$onClick(
					elm$json$Json$Decode$succeed(
						toMsg(updState))) : elm$core$Platform$Sub$none
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$subscriptions = F2(
	function (state, toMsg) {
		var visibility = state.a.visibility;
		var updState = function (v) {
			return A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
				function (s) {
					return _Utils_update(
						s,
						{visibility: v});
				},
				state);
		};
		return elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					function () {
					switch (visibility.$) {
						case 'StartDown':
							return elm$browser$Browser$Events$onAnimationFrame(
								function (_n1) {
									return toMsg(
										updState(rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingDown));
								});
						case 'StartUp':
							return elm$browser$Browser$Events$onAnimationFrame(
								function (_n2) {
									return toMsg(
										updState(rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingUp));
								});
						default:
							return elm$core$Platform$Sub$none;
					}
				}(),
					elm$browser$Browser$Events$onResize(
					F2(
						function (x, _n3) {
							return toMsg(
								A2(
									rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
									function (s) {
										return _Utils_update(
											s,
											{
												windowWidth: elm$core$Maybe$Just(x)
											});
									},
									state));
						})),
					A2(rundis$elm_bootstrap$Bootstrap$Navbar$dropdownSubscriptions, state, toMsg)
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Tab$subscriptions = F2(
	function (_n0, toMsg) {
		var state = _n0.a;
		var _n1 = state.visibility;
		if (_n1.$ === 'Start') {
			return elm$browser$Browser$Events$onAnimationFrame(
				function (_n2) {
					return toMsg(
						rundis$elm_bootstrap$Bootstrap$Tab$State(
							_Utils_update(
								state,
								{visibility: rundis$elm_bootstrap$Bootstrap$Tab$Showing})));
				});
		} else {
			return elm$core$Platform$Sub$none;
		}
	});
var author$project$Main$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(rundis$elm_bootstrap$Bootstrap$Navbar$subscriptions, model.navbarState, author$project$Main$NavbarMsg),
				A2(rundis$elm_bootstrap$Bootstrap$Carousel$subscriptions, model.carouselState, author$project$Main$CarouselMsg),
				A2(rundis$elm_bootstrap$Bootstrap$Tab$subscriptions, model.tabState, author$project$Main$TabMsg)
			]));
};
var author$project$Routes$NotFound = {$: 'NotFound'};
var author$project$Routes$Home = {$: 'Home'};
var author$project$Routes$Interests = {$: 'Interests'};
var author$project$Routes$ReadMenu = {$: 'ReadMenu'};
var author$project$Routes$ReadPage = function (a) {
	return {$: 'ReadPage', a: a};
};
var elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var elm$url$Url$Parser$mapState = F2(
	function (func, _n0) {
		var visited = _n0.visited;
		var unvisited = _n0.unvisited;
		var params = _n0.params;
		var frag = _n0.frag;
		var value = _n0.value;
		return A5(
			elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var elm$url$Url$Parser$map = F2(
	function (subValue, _n0) {
		var parseArg = _n0.a;
		return elm$url$Url$Parser$Parser(
			function (_n1) {
				var visited = _n1.visited;
				var unvisited = _n1.unvisited;
				var params = _n1.params;
				var frag = _n1.frag;
				var value = _n1.value;
				return A2(
					elm$core$List$map,
					elm$url$Url$Parser$mapState(value),
					parseArg(
						A5(elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$url$Url$Parser$oneOf = function (parsers) {
	return elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				elm$core$List$concatMap,
				function (_n0) {
					var parser = _n0.a;
					return parser(state);
				},
				parsers);
		});
};
var elm$url$Url$Parser$s = function (str) {
	return elm$url$Url$Parser$Parser(
		function (_n0) {
			var visited = _n0.visited;
			var unvisited = _n0.unvisited;
			var params = _n0.params;
			var frag = _n0.frag;
			var value = _n0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						elm$url$Url$Parser$State,
						A2(elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var elm$url$Url$Parser$slash = F2(
	function (_n0, _n1) {
		var parseBefore = _n0.a;
		var parseAfter = _n1.a;
		return elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return elm$url$Url$Parser$Parser(
			function (_n0) {
				var visited = _n0.visited;
				var unvisited = _n0.unvisited;
				var params = _n0.params;
				var frag = _n0.frag;
				var value = _n0.value;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _n2 = stringToSomething(next);
					if (_n2.$ === 'Just') {
						var nextValue = _n2.a;
						return _List_fromArray(
							[
								A5(
								elm$url$Url$Parser$State,
								A2(elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var elm$url$Url$Parser$string = A2(elm$url$Url$Parser$custom, 'STRING', elm$core$Maybe$Just);
var elm$url$Url$Parser$top = elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var author$project$Routes$routeParser = elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2(elm$url$Url$Parser$map, author$project$Routes$Home, elm$url$Url$Parser$top),
			A2(
			elm$url$Url$Parser$map,
			author$project$Routes$About,
			elm$url$Url$Parser$s('About')),
			A2(
			elm$url$Url$Parser$map,
			author$project$Routes$Interests,
			elm$url$Url$Parser$s('Interests')),
			A2(
			elm$url$Url$Parser$map,
			author$project$Routes$ReadMenu,
			elm$url$Url$Parser$s('Reads')),
			A2(
			elm$url$Url$Parser$map,
			author$project$Routes$ReadPage,
			A2(
				elm$url$Url$Parser$slash,
				elm$url$Url$Parser$s('Reads'),
				elm$url$Url$Parser$string))
		]));
var elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _n1 = state.unvisited;
			if (!_n1.b) {
				return elm$core$Maybe$Just(state.value);
			} else {
				if ((_n1.a === '') && (!_n1.b.b)) {
					return elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				elm$core$List$cons,
				segment,
				elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var elm$url$Url$Parser$preparePath = function (path) {
	var _n0 = A2(elm$core$String$split, '/', path);
	if (_n0.b && (_n0.a === '')) {
		var segments = _n0.b;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _n0;
		return elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (_n0.$ === 'Just') {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$url$Url$percentDecode = _Url_percentDecode;
var elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return elm$core$Maybe$Just(
				A2(elm$core$List$cons, value, list));
		}
	});
var elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _n0 = A2(elm$core$String$split, '=', segment);
		if ((_n0.b && _n0.b.b) && (!_n0.b.b.b)) {
			var rawKey = _n0.a;
			var _n1 = _n0.b;
			var rawValue = _n1.a;
			var _n2 = elm$url$Url$percentDecode(rawKey);
			if (_n2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _n2.a;
				var _n3 = elm$url$Url$percentDecode(rawValue);
				if (_n3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _n3.a;
					return A3(
						elm$core$Dict$update,
						key,
						elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			elm$core$List$foldr,
			elm$url$Url$Parser$addParam,
			elm$core$Dict$empty,
			A2(elm$core$String$split, '&', qry));
	}
};
var elm$url$Url$Parser$parse = F2(
	function (_n0, url) {
		var parser = _n0.a;
		return elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					elm$url$Url$Parser$State,
					_List_Nil,
					elm$url$Url$Parser$preparePath(url.path),
					elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					elm$core$Basics$identity)));
	});
var author$project$Routes$decode = function (url) {
	return A2(elm$url$Url$Parser$parse, author$project$Routes$routeParser, url);
};
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Main$urlUpdate = F2(
	function (url, model) {
		var _n0 = author$project$Routes$decode(url);
		if (_n0.$ === 'Nothing') {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{page: author$project$Routes$NotFound}),
				elm$core$Platform$Cmd$none);
		} else {
			var page = _n0.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{page: page}),
				elm$core$Platform$Cmd$none);
		}
	});
var elm$browser$Browser$Navigation$load = _Browser_load;
var elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + elm$core$String$fromInt(port_));
		}
	});
var elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var elm$url$Url$toString = function (url) {
	var http = function () {
		var _n0 = url.protocol;
		if (_n0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var rundis$elm_bootstrap$Bootstrap$Carousel$Animating = function (a) {
	return {$: 'Animating', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$Paused = {$: 'Paused'};
var rundis$elm_bootstrap$Bootstrap$Carousel$Start = function (a) {
	return {$: 'Start', a: a};
};
var elm$core$Basics$modBy = _Basics_modBy;
var rundis$elm_bootstrap$Bootstrap$Carousel$nextIndex = F3(
	function (stage, currentIndex, size) {
		var helper = function (transition) {
			switch (transition.$) {
				case 'Next':
					return A2(elm$core$Basics$modBy, size, currentIndex + 1);
				case 'Prev':
					return A2(elm$core$Basics$modBy, size, currentIndex - 1);
				default:
					var m = transition.a;
					return A2(elm$core$Basics$modBy, size, m);
			}
		};
		switch (stage.$) {
			case 'Start':
				var transition = stage.a;
				return helper(transition);
			case 'Animating':
				var transition = stage.a;
				return helper(transition);
			default:
				return currentIndex;
		}
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$update = F2(
	function (message, model) {
		var tstage = model.a;
		var settings = model.b;
		var currentIndex = settings.currentIndex;
		var size = settings.size;
		switch (message.$) {
			case 'Pause':
				return A2(
					rundis$elm_bootstrap$Bootstrap$Carousel$State,
					tstage,
					_Utils_update(
						settings,
						{cycling: rundis$elm_bootstrap$Bootstrap$Carousel$Paused}));
			case 'Cycle':
				return A2(
					rundis$elm_bootstrap$Bootstrap$Carousel$State,
					tstage,
					_Utils_update(
						settings,
						{cycling: rundis$elm_bootstrap$Bootstrap$Carousel$Active}));
			case 'SetHover':
				var hovering = message.a;
				return A2(
					rundis$elm_bootstrap$Bootstrap$Carousel$State,
					tstage,
					_Utils_update(
						settings,
						{hovering: hovering}));
			case 'StartTransition':
				var transition = message.a;
				var newSettings = function () {
					var _n2 = settings.cycling;
					if (_n2.$ === 'WaitForUser') {
						return _Utils_update(
							settings,
							{cycling: rundis$elm_bootstrap$Bootstrap$Carousel$Active});
					} else {
						return settings;
					}
				}();
				if (tstage.$ === 'NotAnimating') {
					return (!_Utils_eq(
						A3(
							rundis$elm_bootstrap$Bootstrap$Carousel$nextIndex,
							rundis$elm_bootstrap$Bootstrap$Carousel$Start(transition),
							currentIndex,
							size),
						currentIndex)) ? A2(
						rundis$elm_bootstrap$Bootstrap$Carousel$State,
						rundis$elm_bootstrap$Bootstrap$Carousel$Start(transition),
						newSettings) : A2(rundis$elm_bootstrap$Bootstrap$Carousel$State, tstage, newSettings);
				} else {
					return A2(rundis$elm_bootstrap$Bootstrap$Carousel$State, tstage, newSettings);
				}
			case 'SetAnimating':
				switch (tstage.$) {
					case 'NotAnimating':
						return model;
					case 'Start':
						var transition = tstage.a;
						return A2(
							rundis$elm_bootstrap$Bootstrap$Carousel$State,
							rundis$elm_bootstrap$Bootstrap$Carousel$Animating(transition),
							settings);
					default:
						var transition = tstage.a;
						return A2(
							rundis$elm_bootstrap$Bootstrap$Carousel$State,
							rundis$elm_bootstrap$Bootstrap$Carousel$Animating(transition),
							settings);
				}
			default:
				var size_ = message.a;
				if (tstage.$ === 'NotAnimating') {
					return A2(
						rundis$elm_bootstrap$Bootstrap$Carousel$State,
						rundis$elm_bootstrap$Bootstrap$Carousel$NotAnimating,
						_Utils_update(
							settings,
							{size: size_}));
				} else {
					return A2(
						rundis$elm_bootstrap$Bootstrap$Carousel$State,
						rundis$elm_bootstrap$Bootstrap$Carousel$NotAnimating,
						_Utils_update(
							settings,
							{
								currentIndex: A3(rundis$elm_bootstrap$Bootstrap$Carousel$nextIndex, tstage, currentIndex, size),
								size: size
							}));
				}
		}
	});
var author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'NavbarMsg':
				var state = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{navbarState: state}),
					elm$core$Platform$Cmd$none);
			case 'PageSwitch':
				var page = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{page: page}),
					elm$core$Platform$Cmd$none);
			case 'LinkClicked':
				var urlRequest = msg.a;
				if (urlRequest.$ === 'Internal') {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						A2(
							elm$browser$Browser$Navigation$pushUrl,
							model.key,
							elm$url$Url$toString(url)));
				} else {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						elm$browser$Browser$Navigation$load(url));
				}
			case 'UrlChanged':
				var url = msg.a;
				return A2(author$project$Main$urlUpdate, url, model);
			case 'CarouselMsg':
				var subMsg = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							carouselState: A2(rundis$elm_bootstrap$Bootstrap$Carousel$update, subMsg, model.carouselState)
						}),
					elm$core$Platform$Cmd$none);
			case 'TabMsg':
				var state = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{tabState: state}),
					elm$core$Platform$Cmd$none);
			default:
				var lang = msg.a;
				var curPage = model.page;
				var _n2 = model.page;
				if (_n2.$ === 'About') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								aboutState: {languageSelection: lang}
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
		}
	});
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var author$project$Styles$backgroundColor = elm$html$Html$Attributes$style('background-color');
var author$project$Styles$backgroundImage = elm$html$Html$Attributes$style('background-image');
var author$project$Styles$backgroundPosition = elm$html$Html$Attributes$style('background-position');
var author$project$Styles$backgroundSize = elm$html$Html$Attributes$style('background-size');
var author$project$Styles$color = elm$html$Html$Attributes$style('color');
var author$project$Styles$fontFamily = elm$html$Html$Attributes$style('fontFamily');
var author$project$Styles$fontSize = elm$html$Html$Attributes$style('fontSize');
var author$project$Styles$fontStyle = elm$html$Html$Attributes$style('fontStyle');
var author$project$Styles$lightGrey = '#D3D3D3';
var author$project$Styles$marginTop = elm$html$Html$Attributes$style('margin-top');
var author$project$Styles$minHeight = elm$html$Html$Attributes$style('min-height');
var author$project$Styles$minWidth = elm$html$Html$Attributes$style('min-width');
var author$project$Styles$myFonts = author$project$Styles$fontFamily('sans-serif');
var elm$html$Html$span = _VirtualDom_node('span');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$button = _VirtualDom_node('button');
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$json$Json$Encode$string = _Json_wrap;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm$html$Html$Attributes$classList = function (classes) {
	return elm$html$Html$Attributes$class(
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$map,
				elm$core$Tuple$first,
				A2(elm$core$List$filter, elm$core$Tuple$second, classes))));
};
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption = function (size) {
	switch (size.$) {
		case 'XS':
			return elm$core$Maybe$Nothing;
		case 'SM':
			return elm$core$Maybe$Just('sm');
		case 'MD':
			return elm$core$Maybe$Just('md');
		case 'LG':
			return elm$core$Maybe$Just('lg');
		default:
			return elm$core$Maybe$Just('xl');
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size = modifier.a;
				return _Utils_update(
					options,
					{
						size: elm$core$Maybe$Just(size)
					});
			case 'Coloring':
				var coloring = modifier.a;
				return _Utils_update(
					options,
					{
						coloring: elm$core$Maybe$Just(coloring)
					});
			case 'Block':
				return _Utils_update(
					options,
					{block: true});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			default:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions = {attributes: _List_Nil, block: false, coloring: elm$core$Maybe$Nothing, disabled: false, size: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass = function (role) {
	switch (role.$) {
		case 'Primary':
			return 'primary';
		case 'Secondary':
			return 'secondary';
		case 'Success':
			return 'success';
		case 'Info':
			return 'info';
		case 'Warning':
			return 'warning';
		case 'Danger':
			return 'danger';
		case 'Dark':
			return 'dark';
		case 'Light':
			return 'light';
		default:
			return 'link';
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier, rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('btn', true),
						_Utils_Tuple2('btn-block', options.block),
						_Utils_Tuple2('disabled', options.disabled)
					])),
				elm$html$Html$Attributes$disabled(options.disabled)
			]),
		_Utils_ap(
			function () {
				var _n0 = A2(elm$core$Maybe$andThen, rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption, options.size);
				if (_n0.$ === 'Just') {
					var s = _n0.a;
					return _List_fromArray(
						[
							elm$html$Html$Attributes$class('btn-' + s)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _n1 = options.coloring;
					if (_n1.$ === 'Just') {
						if (_n1.a.$ === 'Roled') {
							var role = _n1.a.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'btn-' + rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						} else {
							var role = _n1.a.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'btn-outline-' + rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						}
					} else {
						return _List_Nil;
					}
				}(),
				options.attributes)));
};
var rundis$elm_bootstrap$Bootstrap$Button$button = F2(
	function (options, children) {
		return A2(
			elm$html$Html$button,
			rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring = function (a) {
	return {$: 'Coloring', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Dark = {$: 'Dark'};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled = function (a) {
	return {$: 'Roled', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Button$dark = rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Button$Dark));
var rundis$elm_bootstrap$Bootstrap$General$Internal$LG = {$: 'LG'};
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Size = function (a) {
	return {$: 'Size', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Button$large = rundis$elm_bootstrap$Bootstrap$Internal$Button$Size(rundis$elm_bootstrap$Bootstrap$General$Internal$LG);
var elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs = function (a) {
	return {$: 'Attrs', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Button$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs(attrs_);
};
var rundis$elm_bootstrap$Bootstrap$Button$onClick = function (message) {
	return rundis$elm_bootstrap$Bootstrap$Button$attrs(
		_List_fromArray(
			[
				A2(
				elm$html$Html$Events$preventDefaultOn,
				'click',
				elm$json$Json$Decode$succeed(
					_Utils_Tuple2(message, true)))
			]));
};
var rundis$elm_bootstrap$Bootstrap$Carousel$Config = function (a) {
	return {$: 'Config', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$config = F2(
	function (toMsg, attributes) {
		return rundis$elm_bootstrap$Bootstrap$Carousel$Config(
			{attributes: attributes, controls: false, indicators: false, slides: _List_Nil, toMsg: toMsg});
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$slides = F2(
	function (newSlides, _n0) {
		var settings = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Carousel$Config(
			_Utils_update(
				settings,
				{slides: newSlides}));
	});
var elm$html$Html$div = _VirtualDom_node('div');
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onMouseEnter = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseenter',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseleave',
		elm$json$Json$Decode$succeed(msg));
};
var rundis$elm_bootstrap$Bootstrap$Carousel$EndTransition = function (a) {
	return {$: 'EndTransition', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$SetHover = function (a) {
	return {$: 'SetHover', a: a};
};
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var rundis$elm_bootstrap$Bootstrap$Carousel$controlNext = A2(
	elm$html$Html$button,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('btn btn-link carousel-control-next'),
			A2(elm$html$Html$Attributes$attribute, 'role', 'button'),
			elm$html$Html$Events$onClick(
			rundis$elm_bootstrap$Bootstrap$Carousel$StartTransition(rundis$elm_bootstrap$Bootstrap$Carousel$Next))
		]),
	_List_fromArray(
		[
			A2(
			elm$html$Html$span,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('carousel-control-next-icon'),
					A2(elm$html$Html$Attributes$attribute, 'aria-hidden', 'true')
				]),
			_List_Nil),
			A2(
			elm$html$Html$span,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('sr-only')
				]),
			_List_fromArray(
				[
					elm$html$Html$text('Next')
				]))
		]));
var rundis$elm_bootstrap$Bootstrap$Carousel$Prev = {$: 'Prev'};
var rundis$elm_bootstrap$Bootstrap$Carousel$controlPrev = A2(
	elm$html$Html$button,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('btn btn-link carousel-control-prev'),
			elm$html$Html$Events$onClick(
			rundis$elm_bootstrap$Bootstrap$Carousel$StartTransition(rundis$elm_bootstrap$Bootstrap$Carousel$Prev))
		]),
	_List_fromArray(
		[
			A2(
			elm$html$Html$span,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('carousel-control-prev-icon'),
					A2(elm$html$Html$Attributes$attribute, 'aria-hidden', 'true')
				]),
			_List_Nil),
			A2(
			elm$html$Html$span,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('sr-only')
				]),
			_List_fromArray(
				[
					elm$html$Html$text('Previous')
				]))
		]));
var elm$html$Html$img = _VirtualDom_node('img');
var elm$html$Html$Attributes$src = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$Keyed$node = elm$virtual_dom$VirtualDom$keyedNode;
var rundis$elm_bootstrap$Bootstrap$Carousel$dirtyHack = function (size) {
	return A3(
		elm$html$Html$Keyed$node,
		'div',
		_List_Nil,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'dirtyHack',
				A2(
					elm$html$Html$img,
					_List_fromArray(
						[
							A2(
							elm$html$Html$Events$on,
							'load',
							elm$json$Json$Decode$succeed(
								rundis$elm_bootstrap$Bootstrap$Carousel$EndTransition(size))),
							elm$html$Html$Attributes$src('https://package.elm-lang.org/assets/favicon.ico'),
							A2(elm$html$Html$Attributes$style, 'display', 'none')
						]),
					_List_Nil))
			]));
};
var elm$html$Html$li = _VirtualDom_node('li');
var elm$html$Html$ol = _VirtualDom_node('ol');
var rundis$elm_bootstrap$Bootstrap$Carousel$Number = function (a) {
	return {$: 'Number', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$indicators = F2(
	function (size, activeIndex) {
		var item = function (n) {
			return A2(
				elm$html$Html$li,
				_List_fromArray(
					[
						elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'active',
								_Utils_eq(n, activeIndex))
							])),
						elm$html$Html$Events$onClick(
						rundis$elm_bootstrap$Bootstrap$Carousel$StartTransition(
							rundis$elm_bootstrap$Bootstrap$Carousel$Number(n)))
					]),
				_List_Nil);
		};
		var items = A2(
			elm$core$List$map,
			item,
			A2(elm$core$List$range, 0, size - 1));
		return A2(
			elm$html$Html$ol,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('carousel-indicators')
				]),
			items);
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$transitionClassNames = F2(
	function (currentIndex, transition) {
		var base = 'carousel-item';
		var leftNext = {directionalClassName: base + '-left', orderClassName: base + '-next'};
		var rightPrev = {directionalClassName: base + '-right', orderClassName: base + '-prev'};
		switch (transition.$) {
			case 'Next':
				return leftNext;
			case 'Number':
				var n = transition.a;
				return (_Utils_cmp(n, currentIndex) > 0) ? leftNext : rightPrev;
			default:
				return rightPrev;
		}
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$transitionClasses = F4(
	function (index, currentIndex, newIndex, tstage) {
		if (_Utils_eq(index, currentIndex)) {
			switch (tstage.$) {
				case 'NotAnimating':
					return _List_fromArray(
						[
							_Utils_Tuple2('active', true)
						]);
				case 'Start':
					var transition = tstage.a;
					return _List_fromArray(
						[
							_Utils_Tuple2('active', true)
						]);
				default:
					var transition = tstage.a;
					var _n1 = A2(rundis$elm_bootstrap$Bootstrap$Carousel$transitionClassNames, currentIndex, transition);
					var directionalClassName = _n1.directionalClassName;
					return _List_fromArray(
						[
							_Utils_Tuple2('active', true),
							_Utils_Tuple2(directionalClassName, true)
						]);
			}
		} else {
			if (_Utils_eq(index, newIndex)) {
				switch (tstage.$) {
					case 'NotAnimating':
						return _List_Nil;
					case 'Start':
						var transition = tstage.a;
						return _List_fromArray(
							[
								_Utils_Tuple2(
								function ($) {
									return $.orderClassName;
								}(
									A2(rundis$elm_bootstrap$Bootstrap$Carousel$transitionClassNames, currentIndex, transition)),
								true)
							]);
					default:
						var transition = tstage.a;
						var _n3 = A2(rundis$elm_bootstrap$Bootstrap$Carousel$transitionClassNames, currentIndex, transition);
						var directionalClassName = _n3.directionalClassName;
						var orderClassName = _n3.orderClassName;
						return _List_fromArray(
							[
								_Utils_Tuple2(directionalClassName, true),
								_Utils_Tuple2(orderClassName, true)
							]);
				}
			} else {
				return _List_Nil;
			}
		}
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$Config = function (a) {
	return {$: 'Config', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$addAttributes = F2(
	function (newAttributes, _n0) {
		var settings = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$Config(
			_Utils_update(
				settings,
				{
					attributes: _Utils_ap(settings.attributes, newAttributes)
				}));
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$view = function (_n0) {
	var attributes = _n0.a.attributes;
	var content = _n0.a.content;
	var caption = _n0.a.caption;
	var captionHtml = function () {
		if (caption.$ === 'Nothing') {
			return elm$html$Html$text('');
		} else {
			var rec = caption.a;
			return A2(
				elm$html$Html$div,
				_Utils_ap(
					rec.attributes,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('carousel-caption d-none d-md-block')
						])),
				rec.children);
		}
	}();
	return A2(
		elm$html$Html$div,
		_Utils_ap(
			attributes,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('carousel-item')
				])),
		function () {
			if (content.$ === 'Image') {
				var rec = content.a;
				return _List_fromArray(
					[
						A2(
						elm$html$Html$img,
						_Utils_ap(
							rec.attributes,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class('d-block img-fluid'),
									elm$html$Html$Attributes$src(rec.src)
								])),
						_List_Nil),
						captionHtml
					]);
			} else {
				var html = content.a.html;
				return _List_fromArray(
					[html, captionHtml]);
			}
		}());
};
var rundis$elm_bootstrap$Bootstrap$Carousel$viewSlide = F3(
	function (model, index, slide) {
		var tstage = model.a;
		var currentIndex = model.b.currentIndex;
		var size = model.b.size;
		var newIndex = A3(rundis$elm_bootstrap$Bootstrap$Carousel$nextIndex, tstage, currentIndex, size);
		return rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$view(
			A2(
				rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$addAttributes,
				_List_fromArray(
					[
						elm$html$Html$Attributes$classList(
						A4(rundis$elm_bootstrap$Bootstrap$Carousel$transitionClasses, index, currentIndex, newIndex, tstage))
					]),
				slide));
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$view = F2(
	function (model, _n0) {
		var tstage = model.a;
		var hovering = model.b.hovering;
		var currentIndex = model.b.currentIndex;
		var wrap = model.b.wrap;
		var settings = _n0.a;
		var slidesHtml = A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('carousel-inner'),
					A2(elm$html$Html$Attributes$attribute, 'role', 'listbox')
				]),
			A2(
				elm$core$List$indexedMap,
				rundis$elm_bootstrap$Bootstrap$Carousel$viewSlide(model),
				settings.slides));
		var size = elm$core$List$length(settings.slides);
		var indicatorsHtml = settings.indicators ? A2(
			rundis$elm_bootstrap$Bootstrap$Carousel$indicators,
			size,
			A3(rundis$elm_bootstrap$Bootstrap$Carousel$nextIndex, tstage, currentIndex, size)) : elm$html$Html$text('');
		var defaultCarouselAttributes = _Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('carousel slide'),
					A2(
					elm$html$Html$Events$on,
					'transitionend',
					elm$json$Json$Decode$succeed(
						settings.toMsg(
							rundis$elm_bootstrap$Bootstrap$Carousel$EndTransition(size))))
				]),
			(!_Utils_eq(hovering, rundis$elm_bootstrap$Bootstrap$Carousel$IgnoreHover)) ? _List_fromArray(
				[
					elm$html$Html$Events$onMouseEnter(
					settings.toMsg(
						rundis$elm_bootstrap$Bootstrap$Carousel$SetHover(rundis$elm_bootstrap$Bootstrap$Carousel$Hovered))),
					elm$html$Html$Events$onMouseLeave(
					settings.toMsg(
						rundis$elm_bootstrap$Bootstrap$Carousel$SetHover(rundis$elm_bootstrap$Bootstrap$Carousel$NotHovered)))
				]) : _List_Nil);
		var controlsHtml = settings.controls ? ((wrap || (currentIndex && (!_Utils_eq(currentIndex, size - 1)))) ? _List_fromArray(
			[rundis$elm_bootstrap$Bootstrap$Carousel$controlPrev, rundis$elm_bootstrap$Bootstrap$Carousel$controlNext]) : ((!currentIndex) ? _List_fromArray(
			[rundis$elm_bootstrap$Bootstrap$Carousel$controlNext]) : _List_fromArray(
			[rundis$elm_bootstrap$Bootstrap$Carousel$controlPrev]))) : _List_Nil;
		return A2(
			elm$html$Html$div,
			_Utils_ap(settings.attributes, defaultCarouselAttributes),
			A2(
				elm$core$List$cons,
				slidesHtml,
				A2(
					elm$core$List$map,
					elm$html$Html$map(settings.toMsg),
					_Utils_ap(
						_List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$Carousel$dirtyHack(size),
								indicatorsHtml
							]),
						controlsHtml))));
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$Slide$config = F2(
	function (attributes, content) {
		return rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$Config(
			{attributes: attributes, caption: elm$core$Maybe$Nothing, content: content});
	});
var rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Carousel$Slide$customContent = function (html) {
	return rundis$elm_bootstrap$Bootstrap$Carousel$SlideInternal$Custom(
		{html: html});
};
var rundis$elm_bootstrap$Bootstrap$Grid$Column = function (a) {
	return {$: 'Column', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$col = F2(
	function (options, children) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Column(
			{children: children, options: options});
	});
var rundis$elm_bootstrap$Bootstrap$Grid$container = F2(
	function (attributes, children) {
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('container')
					]),
				attributes),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$General$Internal$XS = {$: 'XS'};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col = {$: 'Col'};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width = F2(
	function (screenSize, columnCount) {
		return {columnCount: columnCount, screenSize: screenSize};
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign = F2(
	function (align_, options) {
		var _n0 = align_.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						alignXs: elm$core$Maybe$Just(align_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						alignSm: elm$core$Maybe$Just(align_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						alignMd: elm$core$Maybe$Just(align_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						alignLg: elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						alignXl: elm$core$Maybe$Just(align_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset = F2(
	function (offset_, options) {
		var _n0 = offset_.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						offsetXs: elm$core$Maybe$Just(offset_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						offsetSm: elm$core$Maybe$Just(offset_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						offsetMd: elm$core$Maybe$Just(offset_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						offsetLg: elm$core$Maybe$Just(offset_)
					});
			default:
				return _Utils_update(
					options,
					{
						offsetXl: elm$core$Maybe$Just(offset_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder = F2(
	function (order_, options) {
		var _n0 = order_.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						orderXs: elm$core$Maybe$Just(order_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						orderSm: elm$core$Maybe$Just(order_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						orderMd: elm$core$Maybe$Just(order_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						orderLg: elm$core$Maybe$Just(order_)
					});
			default:
				return _Utils_update(
					options,
					{
						orderXl: elm$core$Maybe$Just(order_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull = F2(
	function (pull_, options) {
		var _n0 = pull_.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						pullXs: elm$core$Maybe$Just(pull_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						pullSm: elm$core$Maybe$Just(pull_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						pullMd: elm$core$Maybe$Just(pull_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						pullLg: elm$core$Maybe$Just(pull_)
					});
			default:
				return _Utils_update(
					options,
					{
						pullXl: elm$core$Maybe$Just(pull_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush = F2(
	function (push_, options) {
		var _n0 = push_.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						pushXs: elm$core$Maybe$Just(push_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						pushSm: elm$core$Maybe$Just(push_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						pushMd: elm$core$Maybe$Just(push_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						pushLg: elm$core$Maybe$Just(push_)
					});
			default:
				return _Utils_update(
					options,
					{
						pushXl: elm$core$Maybe$Just(push_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth = F2(
	function (width_, options) {
		var _n0 = width_.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						widthXs: elm$core$Maybe$Just(width_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						widthSm: elm$core$Maybe$Just(width_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						widthMd: elm$core$Maybe$Just(width_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						widthLg: elm$core$Maybe$Just(width_)
					});
			default:
				return _Utils_update(
					options,
					{
						widthXl: elm$core$Maybe$Just(width_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'ColAttrs':
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
			case 'ColWidth':
				var width_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth, width_, options);
			case 'ColOffset':
				var offset_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset, offset_, options);
			case 'ColPull':
				var pull_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull, pull_, options);
			case 'ColPush':
				var push_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush, push_, options);
			case 'ColOrder':
				var order_ = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder, order_, options);
			case 'ColAlign':
				var align = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign, align, options);
			default:
				var align = modifier.a;
				return _Utils_update(
					options,
					{
						textAlign: elm$core$Maybe$Just(align)
					});
		}
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption = function (size) {
	switch (size.$) {
		case 'Col':
			return elm$core$Maybe$Nothing;
		case 'Col1':
			return elm$core$Maybe$Just('1');
		case 'Col2':
			return elm$core$Maybe$Just('2');
		case 'Col3':
			return elm$core$Maybe$Just('3');
		case 'Col4':
			return elm$core$Maybe$Just('4');
		case 'Col5':
			return elm$core$Maybe$Just('5');
		case 'Col6':
			return elm$core$Maybe$Just('6');
		case 'Col7':
			return elm$core$Maybe$Just('7');
		case 'Col8':
			return elm$core$Maybe$Just('8');
		case 'Col9':
			return elm$core$Maybe$Just('9');
		case 'Col10':
			return elm$core$Maybe$Just('10');
		case 'Col11':
			return elm$core$Maybe$Just('11');
		case 'Col12':
			return elm$core$Maybe$Just('12');
		default:
			return elm$core$Maybe$Just('auto');
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass = function (_n0) {
	var screenSize = _n0.screenSize;
	var columnCount = _n0.columnCount;
	return elm$html$Html$Attributes$class(
		'col' + (A2(
			elm$core$Maybe$withDefault,
			'',
			A2(
				elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + A2(
			elm$core$Maybe$withDefault,
			'',
			A2(
				elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption(columnCount)))));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes = function (widths) {
	var width_ = function (w) {
		return A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass, w);
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, width_, widths));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions = {alignLg: elm$core$Maybe$Nothing, alignMd: elm$core$Maybe$Nothing, alignSm: elm$core$Maybe$Nothing, alignXl: elm$core$Maybe$Nothing, alignXs: elm$core$Maybe$Nothing, attributes: _List_Nil, offsetLg: elm$core$Maybe$Nothing, offsetMd: elm$core$Maybe$Nothing, offsetSm: elm$core$Maybe$Nothing, offsetXl: elm$core$Maybe$Nothing, offsetXs: elm$core$Maybe$Nothing, orderLg: elm$core$Maybe$Nothing, orderMd: elm$core$Maybe$Nothing, orderSm: elm$core$Maybe$Nothing, orderXl: elm$core$Maybe$Nothing, orderXs: elm$core$Maybe$Nothing, pullLg: elm$core$Maybe$Nothing, pullMd: elm$core$Maybe$Nothing, pullSm: elm$core$Maybe$Nothing, pullXl: elm$core$Maybe$Nothing, pullXs: elm$core$Maybe$Nothing, pushLg: elm$core$Maybe$Nothing, pushMd: elm$core$Maybe$Nothing, pushSm: elm$core$Maybe$Nothing, pushXl: elm$core$Maybe$Nothing, pushXs: elm$core$Maybe$Nothing, textAlign: elm$core$Maybe$Nothing, widthLg: elm$core$Maybe$Nothing, widthMd: elm$core$Maybe$Nothing, widthSm: elm$core$Maybe$Nothing, widthXl: elm$core$Maybe$Nothing, widthXs: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption = function (size) {
	switch (size.$) {
		case 'Offset0':
			return '0';
		case 'Offset1':
			return '1';
		case 'Offset2':
			return '2';
		case 'Offset3':
			return '3';
		case 'Offset4':
			return '4';
		case 'Offset5':
			return '5';
		case 'Offset6':
			return '6';
		case 'Offset7':
			return '7';
		case 'Offset8':
			return '8';
		case 'Offset9':
			return '9';
		case 'Offset10':
			return '10';
		default:
			return '11';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString = function (screenSize) {
	var _n0 = rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize);
	if (_n0.$ === 'Just') {
		var s = _n0.a;
		return '-' + (s + '-');
	} else {
		return '-';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass = function (_n0) {
	var screenSize = _n0.screenSize;
	var offsetCount = _n0.offsetCount;
	return elm$html$Html$Attributes$class(
		'offset' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption(offsetCount)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes = function (offsets) {
	var offset_ = function (m) {
		return A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass, m);
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, offset_, offsets));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption = function (size) {
	switch (size.$) {
		case 'OrderFirst':
			return 'first';
		case 'Order1':
			return '1';
		case 'Order2':
			return '2';
		case 'Order3':
			return '3';
		case 'Order4':
			return '4';
		case 'Order5':
			return '5';
		case 'Order6':
			return '6';
		case 'Order7':
			return '7';
		case 'Order8':
			return '8';
		case 'Order9':
			return '9';
		case 'Order10':
			return '10';
		case 'Order11':
			return '11';
		case 'Order12':
			return '12';
		default:
			return 'last';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes = function (orders) {
	var order_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class(
					'order' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption(moveCount))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, order_, orders));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption = function (size) {
	switch (size.$) {
		case 'Move0':
			return '0';
		case 'Move1':
			return '1';
		case 'Move2':
			return '2';
		case 'Move3':
			return '3';
		case 'Move4':
			return '4';
		case 'Move5':
			return '5';
		case 'Move6':
			return '6';
		case 'Move7':
			return '7';
		case 'Move8':
			return '8';
		case 'Move9':
			return '9';
		case 'Move10':
			return '10';
		case 'Move11':
			return '11';
		default:
			return '12';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes = function (pulls) {
	var pull_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class(
					'pull' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, pull_, pulls));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes = function (pushes) {
	var push_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return elm$core$Maybe$Just(
				elm$html$Html$Attributes$class(
					'push' + (rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, push_, pushes));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption = function (align) {
	switch (align.$) {
		case 'Top':
			return 'start';
		case 'Middle':
			return 'center';
		default:
			return 'end';
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass = F2(
	function (prefix, _n0) {
		var align = _n0.align;
		var screenSize = _n0.screenSize;
		return elm$html$Html$Attributes$class(
			_Utils_ap(
				prefix,
				_Utils_ap(
					A2(
						elm$core$Maybe$withDefault,
						'',
						A2(
							elm$core$Maybe$map,
							function (v) {
								return v + '-';
							},
							rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))),
					rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption(align))));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes = F2(
	function (prefix, aligns) {
		var align = function (a) {
			return A2(
				elm$core$Maybe$map,
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass(prefix),
				a);
		};
		return A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			A2(elm$core$List$map, align, aligns));
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption = function (dir) {
	switch (dir.$) {
		case 'Center':
			return 'center';
		case 'Left':
			return 'left';
		default:
			return 'right';
	}
};
var rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass = function (_n0) {
	var dir = _n0.dir;
	var size = _n0.size;
	return elm$html$Html$Attributes$class(
		'text' + (A2(
			elm$core$Maybe$withDefault,
			'-',
			A2(
				elm$core$Maybe$map,
				function (s) {
					return '-' + (s + '-');
				},
				rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size))) + rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption(dir)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption, rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions, modifiers);
	var shouldAddDefaultXs = !elm$core$List$length(
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[options.widthXs, options.widthSm, options.widthMd, options.widthLg, options.widthXl])));
	return _Utils_ap(
		rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes(
			_List_fromArray(
				[
					shouldAddDefaultXs ? elm$core$Maybe$Just(
					A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, rundis$elm_bootstrap$Bootstrap$General$Internal$XS, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col)) : options.widthXs,
					options.widthSm,
					options.widthMd,
					options.widthLg,
					options.widthXl
				])),
		_Utils_ap(
			rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes(
				_List_fromArray(
					[options.offsetXs, options.offsetSm, options.offsetMd, options.offsetLg, options.offsetXl])),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes(
					_List_fromArray(
						[options.pullXs, options.pullSm, options.pullMd, options.pullLg, options.pullXl])),
				_Utils_ap(
					rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes(
						_List_fromArray(
							[options.pushXs, options.pushSm, options.pushMd, options.pushLg, options.pushXl])),
					_Utils_ap(
						rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes(
							_List_fromArray(
								[options.orderXs, options.orderSm, options.orderMd, options.orderLg, options.orderXl])),
						_Utils_ap(
							A2(
								rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
								'align-self-',
								_List_fromArray(
									[options.alignXs, options.alignSm, options.alignMd, options.alignLg, options.alignXl])),
							_Utils_ap(
								function () {
									var _n0 = options.textAlign;
									if (_n0.$ === 'Just') {
										var a = _n0.a;
										return _List_fromArray(
											[
												rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(a)
											]);
									} else {
										return _List_Nil;
									}
								}(),
								options.attributes)))))));
};
var rundis$elm_bootstrap$Bootstrap$Grid$renderCol = function (column) {
	switch (column.$) {
		case 'Column':
			var options = column.a.options;
			var children = column.a.children;
			return A2(
				elm$html$Html$div,
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
		case 'ColBreak':
			var e = column.a;
			return e;
		default:
			var options = column.a.options;
			var children = column.a.children;
			return A3(
				elm$html$Html$Keyed$node,
				'div',
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
	}
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign = F2(
	function (align, options) {
		var _n0 = align.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						hAlignXs: elm$core$Maybe$Just(align)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						hAlignSm: elm$core$Maybe$Just(align)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						hAlignMd: elm$core$Maybe$Just(align)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						hAlignLg: elm$core$Maybe$Just(align)
					});
			default:
				return _Utils_update(
					options,
					{
						hAlignXl: elm$core$Maybe$Just(align)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign = F2(
	function (align_, options) {
		var _n0 = align_.screenSize;
		switch (_n0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						vAlignXs: elm$core$Maybe$Just(align_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						vAlignSm: elm$core$Maybe$Just(align_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						vAlignMd: elm$core$Maybe$Just(align_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						vAlignLg: elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						vAlignXl: elm$core$Maybe$Just(align_)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'RowAttrs':
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
			case 'RowVAlign':
				var align = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign, align, options);
			default:
				var align = modifier.a;
				return A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign, align, options);
		}
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions = {attributes: _List_Nil, hAlignLg: elm$core$Maybe$Nothing, hAlignMd: elm$core$Maybe$Nothing, hAlignSm: elm$core$Maybe$Nothing, hAlignXl: elm$core$Maybe$Nothing, hAlignXs: elm$core$Maybe$Nothing, vAlignLg: elm$core$Maybe$Nothing, vAlignMd: elm$core$Maybe$Nothing, vAlignSm: elm$core$Maybe$Nothing, vAlignXl: elm$core$Maybe$Nothing, vAlignXs: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption = function (align) {
	switch (align.$) {
		case 'Left':
			return 'start';
		case 'Center':
			return 'center';
		case 'Right':
			return 'end';
		case 'Around':
			return 'around';
		default:
			return 'between';
	}
};
var rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass = function (_n0) {
	var align = _n0.align;
	var screenSize = _n0.screenSize;
	return elm$html$Html$Attributes$class(
		'justify-content-' + (A2(
			elm$core$Maybe$withDefault,
			'',
			A2(
				elm$core$Maybe$map,
				function (v) {
					return v + '-';
				},
				rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption(align)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes = function (aligns) {
	var align = function (a) {
		return A2(elm$core$Maybe$map, rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass, a);
	};
	return A2(
		elm$core$List$filterMap,
		elm$core$Basics$identity,
		A2(elm$core$List$map, align, aligns));
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes = function (modifiers) {
	var options = A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption, rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('row')
			]),
		_Utils_ap(
			A2(
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
				'align-items-',
				_List_fromArray(
					[options.vAlignXs, options.vAlignSm, options.vAlignMd, options.vAlignLg, options.vAlignXl])),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes(
					_List_fromArray(
						[options.hAlignXs, options.hAlignSm, options.hAlignMd, options.hAlignLg, options.hAlignXl])),
				options.attributes)));
};
var rundis$elm_bootstrap$Bootstrap$Grid$row = F2(
	function (options, cols) {
		return A2(
			elm$html$Html$div,
			rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes(options),
			A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Grid$renderCol, cols));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAttrs = function (a) {
	return {$: 'ColAttrs', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAttrs(attrs_);
};
var rundis$elm_bootstrap$Bootstrap$General$Internal$XL = {$: 'XL'};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Bottom = {$: 'Bottom'};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAlign = function (a) {
	return {$: 'ColAlign', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$VAlign = F2(
	function (screenSize, align) {
		return {align: align, screenSize: screenSize};
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$colVAlign = F2(
	function (size, align) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAlign(
			A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$VAlign, size, align));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Col$bottomXl = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$colVAlign, rundis$elm_bootstrap$Bootstrap$General$Internal$XL, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Bottom);
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Middle = {$: 'Middle'};
var rundis$elm_bootstrap$Bootstrap$Grid$Col$middleXs = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$colVAlign, rundis$elm_bootstrap$Bootstrap$General$Internal$XS, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Middle);
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$TextAlign = function (a) {
	return {$: 'TextAlign', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Col$textAlign = function (align) {
	return rundis$elm_bootstrap$Bootstrap$Grid$Internal$TextAlign(align);
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowAttrs = function (a) {
	return {$: 'RowAttrs', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowAttrs(attrs_);
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowVAlign = function (a) {
	return {$: 'RowVAlign', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowVAlign = F2(
	function (size, align) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowVAlign(
			A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$VAlign, size, align));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Row$bottomXs = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowVAlign, rundis$elm_bootstrap$Bootstrap$General$Internal$XS, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Bottom);
var rundis$elm_bootstrap$Bootstrap$Internal$Text$Center = {$: 'Center'};
var rundis$elm_bootstrap$Bootstrap$Text$alignXs = function (dir) {
	return {dir: dir, size: rundis$elm_bootstrap$Bootstrap$General$Internal$XS};
};
var rundis$elm_bootstrap$Bootstrap$Text$alignXsCenter = rundis$elm_bootstrap$Bootstrap$Text$alignXs(rundis$elm_bootstrap$Bootstrap$Internal$Text$Center);
var author$project$Main$carouselAboutPage = function (model) {
	var textStyle = _List_fromArray(
		[
			author$project$Styles$color(author$project$Styles$lightGrey),
			author$project$Styles$fontSize('30px'),
			author$project$Styles$fontStyle('bold'),
			author$project$Styles$myFonts
		]);
	var myBackground = _List_fromArray(
		[
			author$project$Styles$backgroundImage('url(\"background.jpg\")'),
			author$project$Styles$backgroundColor('darkBlue'),
			author$project$Styles$backgroundPosition('center'),
			author$project$Styles$backgroundSize('cover'),
			author$project$Styles$minWidth('100%'),
			author$project$Styles$minHeight('95.5vh'),
			author$project$Styles$marginTop('3.55%')
		]);
	var carouselSlide = function (msg) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Carousel$Slide$config,
			_List_Nil,
			rundis$elm_bootstrap$Bootstrap$Carousel$Slide$customContent(
				elm$html$Html$text(msg)));
	};
	var carouselView = A2(
		rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs(
								_List_fromArray(
									[
										author$project$Styles$fontFamily('monospace'),
										author$project$Styles$fontSize('2em')
									]))
							]),
						_List_fromArray(
							[
								elm$html$Html$text('λ x → x')
							]))
					])),
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs(
						_List_fromArray(
							[
								author$project$Styles$marginTop('1%')
							]))
					]),
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Carousel$view,
								model.carouselState,
								A2(
									rundis$elm_bootstrap$Bootstrap$Carousel$slides,
									_List_fromArray(
										[
											carouselSlide('Software Developer'),
											carouselSlide('Undergraduate Student'),
											carouselSlide('Enthusiastic Learner')
										]),
									A2(rundis$elm_bootstrap$Bootstrap$Carousel$config, author$project$Main$CarouselMsg, _List_Nil)))
							]))
					])),
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs(
						_List_fromArray(
							[
								author$project$Styles$marginTop('1%')
							]))
					]),
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Grid$Col$bottomXl]),
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										rundis$elm_bootstrap$Bootstrap$Button$large,
										rundis$elm_bootstrap$Bootstrap$Button$dark,
										rundis$elm_bootstrap$Bootstrap$Button$onClick(
										author$project$Main$LinkClicked(
											elm$browser$Browser$External('http://lucasdutton.website/resume.pdf')))
									]),
								_List_fromArray(
									[
										elm$html$Html$text('Resume')
									]))
							]))
					]))
			]));
	return A2(
		rundis$elm_bootstrap$Bootstrap$Grid$row,
		_List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$Grid$Row$bottomXs,
				rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs(myBackground)
			]),
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$col,
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Grid$Col$middleXs,
						rundis$elm_bootstrap$Bootstrap$Grid$Col$textAlign(rundis$elm_bootstrap$Bootstrap$Text$alignXsCenter),
						rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs(textStyle)
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[carouselView]))
					]))
			]));
};
var elm_explorations$markdown$Markdown$defaultOptions = {
	defaultHighlighting: elm$core$Maybe$Nothing,
	githubFlavored: elm$core$Maybe$Just(
		{breaks: false, tables: false}),
	sanitize: true,
	smartypants: false
};
var elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var elm_explorations$markdown$Markdown$toHtmlWith = _Markdown_toHtml;
var elm_explorations$markdown$Markdown$toHtml = elm_explorations$markdown$Markdown$toHtmlWith(elm_explorations$markdown$Markdown$defaultOptions);
var author$project$Styles$toMD = elm_explorations$markdown$Markdown$toHtml(
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('content')
		]));
var elm$html$Html$h4 = _VirtualDom_node('h4');
var elm$html$Html$ul = _VirtualDom_node('ul');
var rundis$elm_bootstrap$Bootstrap$General$Internal$MD = {$: 'MD'};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col6 = {$: 'Col6'};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth = function (a) {
	return {$: 'ColWidth', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$width = F2(
	function (size, count) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth(
			A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, size, count));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Col$md6 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, rundis$elm_bootstrap$Bootstrap$General$Internal$MD, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col6);
var rundis$elm_bootstrap$Bootstrap$General$Internal$Center = {$: 'Center'};
var rundis$elm_bootstrap$Bootstrap$General$Internal$HAlign = F2(
	function (screenSize, align) {
		return {align: align, screenSize: screenSize};
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowHAlign = function (a) {
	return {$: 'RowHAlign', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowHAlign = F2(
	function (size, align) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowHAlign(
			A2(rundis$elm_bootstrap$Bootstrap$General$Internal$HAlign, size, align));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Row$centerXs = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowHAlign, rundis$elm_bootstrap$Bootstrap$General$Internal$XS, rundis$elm_bootstrap$Bootstrap$General$Internal$Center);
var rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt4 = elm$html$Html$Attributes$class('mt-4');
var author$project$Main$aboutBlurb = function () {
	var publications = A2(
		elm$core$List$map,
		function (x) {
			return A2(
				elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						author$project$Styles$toMD(x)
					]));
		},
		_List_fromArray(
			['[ResearchGate](https://dl.acm.org/author_page.cfm?id=99659321607&coll=DL&dl=ACM&trk=0)']));
	var achievements = A2(
		elm$core$List$map,
		function (x) {
			return A2(
				elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						author$project$Styles$toMD(x)
					]));
		},
		_List_fromArray(
			['[CASCON 2018](https://www-01.ibm.com/ibm/cas/cascon/awards.jsp) - Project of the Year', '[CASCON 2018](https://www-01.ibm.com/ibm/cas/cascon/awards.jsp) - Student of the Year - Honorable Mention', 'ECNA ICPC 2018- 31st place']));
	var aboutText = A2(
		elm_explorations$markdown$Markdown$toHtml,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('content')
			]),
		'\nI\'m a third-year software engineering undergraduate studying in McMaster University. I love developing\na wide variety of software, with emphasis on efficiency and safety. On my free time,\nI code as a hobby, play video games, and read fictional books and educational resources\non Category Theory, Abstract Algebra and Algorithms. More in **Interests**!\n');
	return A2(
		elm$html$Html$span,
		_List_fromArray(
			[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt4]),
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs(
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt4]))
					]),
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[aboutText]))
					])),
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Grid$Row$centerXs,
						rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs(
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt4]))
					]),
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Grid$Col$md6]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$h4,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('Publications')
									])),
								A2(
								elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										A2(elm$html$Html$ul, _List_Nil, publications)
									]))
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Grid$Col$md6]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$h4,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('Achievements')
									])),
								A2(
								elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										A2(elm$html$Html$ul, _List_Nil, achievements)
									]))
							]))
					]))
			]));
}();
var author$project$Main$lambda = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nHello there! Wondering what that weird mathematical equation was, complete with that cool looking λ symbol is? Read on!\n\n### Lambda Calculus\n\nThe Lambda Calculus is a model of computation introduced by Alonzo Church. Its syntax consists of three simple\nrules of construction:\n\n- Variables: Strings or characters representing some parameter of a value\n- Abstraction: A function definition, with the syntax (λ x → M). M is another expression, and x is bound in M.\n- Application: Applying functions to arguments, e.g. (M N) is "M is applied to N".\n\n### Identities\n\nIdentities are a vital part of mathematics; One of them is the identity function, defined as \n`f(x) = x`. This means that whatever value we substitute for `x`, the function will return\nthe same output as our input! \n\nIf you now see the correspondence between `f(x) = x`, and `λ x → x`, then yes! We\'ve just seen\nthe identity function written with the lambda calculus!\n\n##### What\'s the point?\n\nI like to think that this website displays my identity, hence my use for this function :)\n\n');
var author$project$Main$LanguageSwitch = function (a) {
	return {$: 'LanguageSwitch', a: a};
};
var author$project$Styles$dimGrey = '#696969';
var author$project$Styles$outline = elm$html$Html$Attributes$style('outline');
var rundis$elm_bootstrap$Bootstrap$Grid$containerFluid = F2(
	function (attributes, children) {
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('container-fluid')
					]),
				attributes),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col3 = {$: 'Col3'};
var rundis$elm_bootstrap$Bootstrap$Grid$Col$md3 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, rundis$elm_bootstrap$Bootstrap$General$Internal$MD, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col3);
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col8 = {$: 'Col8'};
var rundis$elm_bootstrap$Bootstrap$Grid$Col$xs8 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, rundis$elm_bootstrap$Bootstrap$General$Internal$XS, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col8);
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Attrs = function (a) {
	return {$: 'Attrs', a: a};
};
var rundis$elm_bootstrap$Bootstrap$ListGroup$attrs = function (attrs_) {
	return rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Attrs(attrs_);
};
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Action = {$: 'Action'};
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$CustomItem = function (a) {
	return {$: 'CustomItem', a: a};
};
var rundis$elm_bootstrap$Bootstrap$ListGroup$button = F2(
	function (options, children) {
		return rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$CustomItem(
			{
				children: children,
				itemFn: elm$html$Html$button,
				options: A2(
					elm$core$List$cons,
					rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Action,
					_Utils_ap(
						options,
						_List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Attrs(
								_List_fromArray(
									[
										elm$html$Html$Attributes$type_('button')
									]))
							])))
			});
	});
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Roled':
				var role = modifier.a;
				return _Utils_update(
					options,
					{
						role: elm$core$Maybe$Just(role)
					});
			case 'Action':
				return _Utils_update(
					options,
					{action: true});
			case 'Disabled':
				return _Utils_update(
					options,
					{disabled: true});
			case 'Active':
				return _Utils_update(
					options,
					{active: true});
			default:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
		}
	});
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$defaultOptions = {action: false, active: false, attributes: _List_Nil, disabled: false, role: elm$core$Maybe$Nothing};
var rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass = F2(
	function (prefix, role) {
		return elm$html$Html$Attributes$class(
			prefix + ('-' + function () {
				switch (role.$) {
					case 'Primary':
						return 'primary';
					case 'Secondary':
						return 'secondary';
					case 'Success':
						return 'success';
					case 'Info':
						return 'info';
					case 'Warning':
						return 'warning';
					case 'Danger':
						return 'danger';
					case 'Light':
						return 'light';
					default:
						return 'dark';
				}
			}()));
	});
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$itemAttributes = function (options) {
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('list-group-item', true),
						_Utils_Tuple2('disabled', options.disabled),
						_Utils_Tuple2('active', options.active),
						_Utils_Tuple2('list-group-item-action', options.action)
					]))
			]),
		_Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Attributes$disabled(options.disabled)
				]),
			_Utils_ap(
				A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					A2(
						elm$core$Maybe$map,
						function (r) {
							return _List_fromArray(
								[
									A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'list-group-item', r)
								]);
						},
						options.role)),
				options.attributes)));
};
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$renderCustomItem = function (_n0) {
	var itemFn = _n0.a.itemFn;
	var options = _n0.a.options;
	var children = _n0.a.children;
	return A2(
		itemFn,
		rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$itemAttributes(
			A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$applyModifier, rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$defaultOptions, options)),
		children);
};
var rundis$elm_bootstrap$Bootstrap$ListGroup$custom = function (items) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('list-group')
			]),
		A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$renderCustomItem, items));
};
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Roled = function (a) {
	return {$: 'Roled', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Internal$Role$Dark = {$: 'Dark'};
var rundis$elm_bootstrap$Bootstrap$ListGroup$dark = rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Role$Dark);
var author$project$Main$languagesSection = function (model) {
	var pythonBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nThe first language that I learnt. I now use Python for a variety of applications,\nfrom scripting to data visualization.\n\n#### More Information\n- Used Python libraries for visualizing data on floating point errors to understand\nthe behaviour of our floating point implementation on different intervals during\nmy IBM research work.\n- Frequently used to solve specific competitive programming problems, such as String\nand BigInteger problems.\n');
	var page = model.page;
	var selectedLang = function () {
		if (page.$ === 'About') {
			return model.aboutState.languageSelection;
		} else {
			return 'Haskell';
		}
	}();
	var javaBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nMainly used for completing a project in McMaster\'s Software 2XB3 course.\n\n#### More Information\n- Coursework based on finding optimal paths for Uber drivers in cities, using\nmachine learning and graph algorithms.\n');
	var haskellBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nHaskell is my go-to language for just about anything. Yes, I am an avid functional programmer, and\nI firmly believe that **Programming with Types** is beneficial to the programmer - as the saying goes,\nif it compiles, it works!\n\n#### More Information\n- 1 year of research experience of using Haskell for a variety of tasks - Testing, writing and extending DSLs,\n  compilation techniques, faking dependent types.\n- Web development, both front-end and back-end, using frameworks such as [Haste](https://haste-lang.org/) and\n  [PAL](https://github.com/CSchank/petri-app-land).\n- I just enjoy writing code in Haskell in general!\n');
	var elmBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nElm was a language I was introduced to while I had already knew a decent amount of Haskell - and it\nhas given me the opportunity to do front-end web development in my favourite programming paradigm -\nfunctional programming!\n\n#### More Information\n- This site is a redesign of my old static site, which was made using pure HTML, CSS and Javascript. Woah!\n- Elm is surprisingly fast, and competitive with other mainstream front-end Javascript frameworks. See\n  [here](https://elm-lang.org/blog/blazing-fast-html-round-two) for the details.\n');
	var cppBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nC++ is a language I mainly use for competitive programming. It has a rich standard library which contains\nmany important data structures and algorithms, and compiles to really fast code, all which are essential\nfor my application.\n\n#### More Information\n- I have also taken a course in McMaster that used C++ to teach software development practices.\n- C++ is one of my go-to object-oriented programming language, the other being Python.\n');
	var cSharpBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nMainly used when I was developing games with Unity.\n');
	var cBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nC is one of the fundamental languages that taught me about low-level computer intrinsics. I have since\nwritten code in C to deliver algorithmic implementations of my research work to IBM, and taught\na course that used C as the language of choice to teach principles of programming to second-year\nstudents in McMaster University.\n\n#### More Information\n- I have both taken and taught McMaster\'s Software Engineering 2S03 course that uses C.\n- One of the programs that I wrote in C involved image filtering- applying mean and median filters to images\n  using highly optimized code.\n');
	var buttonSelection = function (lang) {
		return _Utils_eq(selectedLang, lang) ? _List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$ListGroup$dark,
				rundis$elm_bootstrap$Bootstrap$ListGroup$attrs(
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						author$project$Main$LanguageSwitch(lang)),
						author$project$Styles$outline('none'),
						author$project$Styles$color(author$project$Styles$dimGrey)
					]))
			]) : _List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$ListGroup$attrs(
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						author$project$Main$LanguageSwitch(lang)),
						author$project$Styles$outline('none'),
						author$project$Styles$color(author$project$Styles$dimGrey)
					]))
			]);
	};
	var languagesList = A2(
		elm$core$List$map,
		function (lang) {
			return A2(
				rundis$elm_bootstrap$Bootstrap$ListGroup$button,
				buttonSelection(lang),
				_List_fromArray(
					[
						elm$html$Html$text(lang)
					]));
		},
		_List_fromArray(
			['Haskell', 'Elm', 'Agda', 'C++', 'C', 'Java', 'Python', 'C#']));
	var agdaBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nMainly used as a theorem prover, Agda is a dependently-typed language which enforces termination of programs.\nMy intererst in Agda is currently mostly academic in nature.\n\n#### Current Focus\n- Learning dependent type theory\n- Learning Agda\'s rich syntax, and standard libraries\n- Agda\'s compiler backend\n');
	var selectedBlurb = function () {
		switch (selectedLang) {
			case 'Elm':
				return elmBlurb;
			case 'Agda':
				return agdaBlurb;
			case 'C++':
				return cppBlurb;
			case 'C':
				return cBlurb;
			case 'Java':
				return javaBlurb;
			case 'Python':
				return pythonBlurb;
			case 'C#':
				return cSharpBlurb;
			default:
				return haskellBlurb;
		}
	}();
	return A2(
		rundis$elm_bootstrap$Bootstrap$Grid$containerFluid,
		_List_fromArray(
			[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt4]),
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Grid$Col$md3]),
						_List_fromArray(
							[
								rundis$elm_bootstrap$Bootstrap$ListGroup$custom(languagesList)
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Grid$Col$xs8]),
						_List_fromArray(
							[selectedBlurb]))
					]))
			]));
};
var elm$html$Html$i = _VirtualDom_node('i');
var author$project$Styles$fontAwesome = function (iconName) {
	return A2(
		elm$html$Html$i,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('fa fa-' + iconName),
				A2(elm$html$Html$Attributes$attribute, 'aria-hidden', 'true')
			]),
		_List_Nil);
};
var elm$html$Html$a = _VirtualDom_node('a');
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var author$project$Styles$fontAwesomeLink = F3(
	function (linkName, iconName, attrs) {
		return A2(
			elm$html$Html$a,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(linkName),
						author$project$Styles$color(author$project$Styles$dimGrey)
					]),
				attrs),
			_List_fromArray(
				[
					author$project$Styles$fontAwesome(iconName)
				]));
	});
var elm$html$Html$h5 = _VirtualDom_node('h5');
var elm$html$Html$p = _VirtualDom_node('p');
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Item = function (a) {
	return {$: 'Item', a: a};
};
var rundis$elm_bootstrap$Bootstrap$ListGroup$li = F2(
	function (options, children) {
		return rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Item(
			{children: children, itemFn: elm$html$Html$li, options: options});
	});
var rundis$elm_bootstrap$Bootstrap$Internal$Role$Light = {$: 'Light'};
var rundis$elm_bootstrap$Bootstrap$ListGroup$light = rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Role$Light);
var rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$renderItem = function (_n0) {
	var itemFn = _n0.a.itemFn;
	var options = _n0.a.options;
	var children = _n0.a.children;
	return A2(
		itemFn,
		rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$itemAttributes(
			A3(elm$core$List$foldl, rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$applyModifier, rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$defaultOptions, options)),
		children);
};
var rundis$elm_bootstrap$Bootstrap$ListGroup$ul = function (items) {
	return A2(
		elm$html$Html$ul,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('list-group')
			]),
		A2(elm$core$List$map, rundis$elm_bootstrap$Bootstrap$Internal$ListGroup$renderItem, items));
};
var rundis$elm_bootstrap$Bootstrap$Utilities$Flex$alignItemsStart = elm$html$Html$Attributes$class('align-items-start');
var rundis$elm_bootstrap$Bootstrap$Utilities$Flex$block = elm$html$Html$Attributes$class('d-flex');
var rundis$elm_bootstrap$Bootstrap$Utilities$Flex$col = elm$html$Html$Attributes$class('flex-column');
var rundis$elm_bootstrap$Bootstrap$Utilities$Flex$justifyBetween = elm$html$Html$Attributes$class('justify-content-between');
var rundis$elm_bootstrap$Bootstrap$Utilities$Size$w100 = elm$html$Html$Attributes$class('w-100');
var rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mb1 = elm$html$Html$Attributes$class('mb-1');
var rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mr2 = elm$html$Html$Attributes$class('mr-2');
var author$project$Main$projectsSection = function (page) {
	var newYouthBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nDeveloped with McMaster Outreach/McMaster Start Coding students, this app was built from the\nground up, beginning from the web framework, [Petri-App-Land](https://github.com/CSchank/petri-app-land).\nUsing Elm and Haskell, we worked with Brampton Multicultural Centre to provide an application for\nnew youths to provide resources and help for integrating into Canada.\n');
	var newYouth = A2(
		rundis$elm_bootstrap$Bootstrap$ListGroup$li,
		_List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$ListGroup$light,
				rundis$elm_bootstrap$Bootstrap$ListGroup$attrs(
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Utilities$Flex$col,
						rundis$elm_bootstrap$Bootstrap$Utilities$Flex$alignItemsStart,
						author$project$Styles$color(author$project$Styles$dimGrey)
					]))
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Flex$block, rundis$elm_bootstrap$Bootstrap$Utilities$Flex$justifyBetween, rundis$elm_bootstrap$Bootstrap$Utilities$Size$w100]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$h5,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mb1]),
						_List_fromArray(
							[
								elm$html$Html$text('NewYouthApp')
							])),
						A2(
						elm$html$Html$h5,
						_List_Nil,
						_List_fromArray(
							[
								A3(author$project$Styles$fontAwesomeLink, 'https://macoutreach.rocks/newyouth/', 'link', _List_Nil)
							]))
					])),
				A2(
				elm$html$Html$p,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mb1]),
				_List_fromArray(
					[newYouthBlurb]))
			]));
	var finsmBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nA lightweight app made with Elm and the GraphicsSVG library to build, simulate and export finite\nstate machines. Successfully used in McMaster\'s second year Finite Automata course by students to submit assignments.\n[finsm.io](finsm.io) was developed as an open-source project in conjunction with another McMaster student.\n');
	var finsm = A2(
		rundis$elm_bootstrap$Bootstrap$ListGroup$li,
		_List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$ListGroup$dark,
				rundis$elm_bootstrap$Bootstrap$ListGroup$attrs(
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Utilities$Flex$col,
						rundis$elm_bootstrap$Bootstrap$Utilities$Flex$alignItemsStart,
						author$project$Styles$color(author$project$Styles$dimGrey)
					]))
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Flex$block, rundis$elm_bootstrap$Bootstrap$Utilities$Flex$justifyBetween, rundis$elm_bootstrap$Bootstrap$Utilities$Size$w100]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$h5,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mb1]),
						_List_fromArray(
							[
								elm$html$Html$text('finsm.io')
							])),
						A2(
						elm$html$Html$h5,
						_List_Nil,
						_List_fromArray(
							[
								A3(
								author$project$Styles$fontAwesomeLink,
								'https://finsm.io',
								'link',
								_List_fromArray(
									[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mr2])),
								A3(author$project$Styles$fontAwesomeLink, 'https://github.com/CSchank/finsm', 'github', _List_Nil)
							]))
					])),
				A2(
				elm$html$Html$p,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mb1]),
				_List_fromArray(
					[finsmBlurb]))
			]));
	var calcCheckBlurb = A2(elm_explorations$markdown$Markdown$toHtml, _List_Nil, '\nA theorem prover developed by Dr. Wolfram Kahl, used by second-year students in the Discrete Mathematics\ncourse. I contributed some front-end improvements and suggestions for the web interface.\n');
	var calcCheck = A2(
		rundis$elm_bootstrap$Bootstrap$ListGroup$li,
		_List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$ListGroup$dark,
				rundis$elm_bootstrap$Bootstrap$ListGroup$attrs(
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Utilities$Flex$col,
						rundis$elm_bootstrap$Bootstrap$Utilities$Flex$alignItemsStart,
						author$project$Styles$color(author$project$Styles$dimGrey)
					]))
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Flex$block, rundis$elm_bootstrap$Bootstrap$Utilities$Flex$justifyBetween, rundis$elm_bootstrap$Bootstrap$Utilities$Size$w100]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$h5,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mb1]),
						_List_fromArray(
							[
								elm$html$Html$text('CalcCheckWeb')
							])),
						A2(
						elm$html$Html$h5,
						_List_Nil,
						_List_fromArray(
							[
								A3(author$project$Styles$fontAwesomeLink, 'http://calccheck.mcmaster.ca', 'link', _List_Nil)
							]))
					])),
				A2(
				elm$html$Html$p,
				_List_fromArray(
					[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mb1]),
				_List_fromArray(
					[calcCheckBlurb]))
			]));
	return A2(
		rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_fromArray(
			[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt4, rundis$elm_bootstrap$Bootstrap$Utilities$Size$w100]),
		_List_fromArray(
			[
				rundis$elm_bootstrap$Bootstrap$ListGroup$ul(
				_List_fromArray(
					[finsm, newYouth, calcCheck]))
			]));
};
var author$project$Styles$grey = '#808080';
var elm$html$Html$h2 = _VirtualDom_node('h2');
var rundis$elm_bootstrap$Bootstrap$Tab$Config = function (a) {
	return {$: 'Config', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Tab$config = function (toMsg) {
	return rundis$elm_bootstrap$Bootstrap$Tab$Config(
		{attributes: _List_Nil, isPill: false, items: _List_Nil, layout: elm$core$Maybe$Nothing, toMsg: toMsg, useHash: false, withAnimation: false});
};
var rundis$elm_bootstrap$Bootstrap$Tab$Fill = {$: 'Fill'};
var rundis$elm_bootstrap$Bootstrap$Tab$layout = F2(
	function (layout_, _n0) {
		var configRec = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Tab$Config(
			_Utils_update(
				configRec,
				{
					layout: elm$core$Maybe$Just(layout_)
				}));
	});
var rundis$elm_bootstrap$Bootstrap$Tab$fill = rundis$elm_bootstrap$Bootstrap$Tab$layout(rundis$elm_bootstrap$Bootstrap$Tab$Fill);
var rundis$elm_bootstrap$Bootstrap$Tab$Item = function (a) {
	return {$: 'Item', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Tab$item = function (rec) {
	return rundis$elm_bootstrap$Bootstrap$Tab$Item(
		{id: rec.id, link: rec.link, pane: rec.pane});
};
var rundis$elm_bootstrap$Bootstrap$Tab$items = F2(
	function (items_, _n0) {
		var configRec = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Tab$Config(
			_Utils_update(
				configRec,
				{items: items_}));
	});
var rundis$elm_bootstrap$Bootstrap$Tab$Link = function (a) {
	return {$: 'Link', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Tab$link = F2(
	function (attributes, children) {
		return rundis$elm_bootstrap$Bootstrap$Tab$Link(
			{attributes: attributes, children: children});
	});
var rundis$elm_bootstrap$Bootstrap$Tab$Pane = function (a) {
	return {$: 'Pane', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Tab$pane = F2(
	function (attributes, children) {
		return rundis$elm_bootstrap$Bootstrap$Tab$Pane(
			{attributes: attributes, children: children});
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var rundis$elm_bootstrap$Bootstrap$Tab$getActiveItem = F2(
	function (_n0, configRec) {
		var activeTab = _n0.a.activeTab;
		if (activeTab.$ === 'Nothing') {
			return elm$core$List$head(configRec.items);
		} else {
			var id = activeTab.a;
			return function (found) {
				if (found.$ === 'Just') {
					var f = found.a;
					return elm$core$Maybe$Just(f);
				} else {
					return elm$core$List$head(configRec.items);
				}
			}(
				elm$core$List$head(
					A2(
						elm$core$List$filter,
						function (_n2) {
							var item_ = _n2.a;
							return _Utils_eq(item_.id, id);
						},
						configRec.items)));
		}
	});
var rundis$elm_bootstrap$Bootstrap$Tab$Hidden = {$: 'Hidden'};
var rundis$elm_bootstrap$Bootstrap$Tab$Start = {$: 'Start'};
var rundis$elm_bootstrap$Bootstrap$Tab$visibilityTransition = F2(
	function (withAnimation_, visibility) {
		var _n0 = _Utils_Tuple2(withAnimation_, visibility);
		_n0$2:
		while (true) {
			if (_n0.a) {
				switch (_n0.b.$) {
					case 'Hidden':
						var _n1 = _n0.b;
						return rundis$elm_bootstrap$Bootstrap$Tab$Start;
					case 'Start':
						var _n2 = _n0.b;
						return rundis$elm_bootstrap$Bootstrap$Tab$Showing;
					default:
						break _n0$2;
				}
			} else {
				break _n0$2;
			}
		}
		return rundis$elm_bootstrap$Bootstrap$Tab$Showing;
	});
var rundis$elm_bootstrap$Bootstrap$Tab$renderLink = F4(
	function (id, active, _n0, configRec) {
		var attributes = _n0.a.attributes;
		var children = _n0.a.children;
		var commonClasses = _List_fromArray(
			[
				_Utils_Tuple2('nav-link', true),
				_Utils_Tuple2('active', active)
			]);
		var clickHandler = elm$html$Html$Events$onClick(
			configRec.toMsg(
				rundis$elm_bootstrap$Bootstrap$Tab$State(
					{
						activeTab: elm$core$Maybe$Just(id),
						visibility: A2(rundis$elm_bootstrap$Bootstrap$Tab$visibilityTransition, configRec.withAnimation && (!active), rundis$elm_bootstrap$Bootstrap$Tab$Hidden)
					})));
		var linkItem = configRec.useHash ? A2(
			elm$html$Html$a,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$classList(commonClasses),
						clickHandler,
						elm$html$Html$Attributes$href('#' + id)
					]),
				attributes),
			children) : A2(
			elm$html$Html$button,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$classList(
						_Utils_ap(
							commonClasses,
							_List_fromArray(
								[
									_Utils_Tuple2('btn', true),
									_Utils_Tuple2('btn-link', true)
								]))),
						clickHandler
					]),
				attributes),
			children);
		return A2(
			elm$html$Html$li,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('nav-item')
				]),
			_List_fromArray(
				[linkItem]));
	});
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var rundis$elm_bootstrap$Bootstrap$Tab$transitionStyles = function (opacity) {
	return _List_fromArray(
		[
			A2(
			elm$html$Html$Attributes$style,
			'opacity',
			elm$core$String$fromInt(opacity)),
			A2(elm$html$Html$Attributes$style, '-webkit-transition', 'opacity 0.15s linear'),
			A2(elm$html$Html$Attributes$style, '-o-transition', 'opacity 0.15s linear'),
			A2(elm$html$Html$Attributes$style, 'transition', 'opacity 0.15s linear')
		]);
};
var rundis$elm_bootstrap$Bootstrap$Tab$activeTabAttributes = F2(
	function (_n0, configRec) {
		var visibility = _n0.a.visibility;
		switch (visibility.$) {
			case 'Hidden':
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'none')
					]);
			case 'Start':
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'block'),
						A2(elm$html$Html$Attributes$style, 'opacity', '0')
					]);
			default:
				return _Utils_ap(
					_List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'display', 'block')
						]),
					rundis$elm_bootstrap$Bootstrap$Tab$transitionStyles(1));
		}
	});
var rundis$elm_bootstrap$Bootstrap$Tab$renderTabPane = F5(
	function (id, active, _n0, state, configRec) {
		var attributes = _n0.a.attributes;
		var children = _n0.a.children;
		var displayAttrs = active ? A2(rundis$elm_bootstrap$Bootstrap$Tab$activeTabAttributes, state, configRec) : _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'display', 'none')
			]);
		return A2(
			elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$id(id),
						elm$html$Html$Attributes$class('tab-pane')
					]),
				_Utils_ap(displayAttrs, attributes)),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes = function (configRec) {
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('nav', true),
						_Utils_Tuple2('nav-tabs', !configRec.isPill),
						_Utils_Tuple2('nav-pills', configRec.isPill)
					]))
			]),
		_Utils_ap(
			function () {
				var _n0 = configRec.layout;
				if (_n0.$ === 'Just') {
					switch (_n0.a.$) {
						case 'Justified':
							var _n1 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('nav-justified')
								]);
						case 'Fill':
							var _n2 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('nav-fill')
								]);
						case 'Center':
							var _n3 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('justify-content-center')
								]);
						default:
							var _n4 = _n0.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class('justify-content-end')
								]);
					}
				} else {
					return _List_Nil;
				}
			}(),
			configRec.attributes));
};
var rundis$elm_bootstrap$Bootstrap$Tab$view = F2(
	function (state, _n0) {
		var configRec = _n0.a;
		var _n1 = A2(rundis$elm_bootstrap$Bootstrap$Tab$getActiveItem, state, configRec);
		if (_n1.$ === 'Nothing') {
			return A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$ul,
						rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes(configRec),
						_List_Nil),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('tab-content')
							]),
						_List_Nil)
					]));
		} else {
			var currentItem = _n1.a.a;
			return A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$ul,
						rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes(configRec),
						A2(
							elm$core$List$map,
							function (_n2) {
								var item_ = _n2.a;
								return A4(
									rundis$elm_bootstrap$Bootstrap$Tab$renderLink,
									item_.id,
									_Utils_eq(item_.id, currentItem.id),
									item_.link,
									configRec);
							},
							configRec.items)),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('tab-content')
							]),
						A2(
							elm$core$List$map,
							function (_n3) {
								var item_ = _n3.a;
								return A5(
									rundis$elm_bootstrap$Bootstrap$Tab$renderTabPane,
									item_.id,
									_Utils_eq(item_.id, currentItem.id),
									item_.pane,
									state,
									configRec);
							},
							configRec.items))
					]));
		}
	});
var rundis$elm_bootstrap$Bootstrap$Tab$withAnimation = function (_n0) {
	var configRec = _n0.a;
	return rundis$elm_bootstrap$Bootstrap$Tab$Config(
		_Utils_update(
			configRec,
			{withAnimation: true}));
};
var author$project$Main$tabsAboutPage = function (model) {
	var paneCustom = _List_fromArray(
		[
			rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mt4,
			author$project$Styles$color(author$project$Styles$dimGrey)
		]);
	var iconCustom = _List_fromArray(
		[
			author$project$Styles$color(author$project$Styles$grey),
			author$project$Styles$fontSize('1.5em')
		]);
	var tabViews = A2(
		rundis$elm_bootstrap$Bootstrap$Tab$view,
		model.tabState,
		A2(
			rundis$elm_bootstrap$Bootstrap$Tab$items,
			_List_fromArray(
				[
					rundis$elm_bootstrap$Bootstrap$Tab$item(
					{
						id: 'About',
						link: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$link,
							iconCustom,
							_List_fromArray(
								[
									author$project$Styles$fontAwesome('info-circle')
								])),
						pane: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$pane,
							paneCustom,
							_List_fromArray(
								[
									A2(
									elm$html$Html$h2,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('About')
										])),
									author$project$Main$aboutBlurb
								]))
					}),
					rundis$elm_bootstrap$Bootstrap$Tab$item(
					{
						id: 'Programming Languages',
						link: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$link,
							iconCustom,
							_List_fromArray(
								[
									author$project$Styles$fontAwesome('code')
								])),
						pane: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$pane,
							paneCustom,
							_List_fromArray(
								[
									A2(
									elm$html$Html$h2,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('Programming Languages')
										])),
									A2(
									elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											author$project$Main$languagesSection(model)
										]))
								]))
					}),
					rundis$elm_bootstrap$Bootstrap$Tab$item(
					{
						id: 'Projects',
						link: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$link,
							iconCustom,
							_List_fromArray(
								[
									author$project$Styles$fontAwesome('puzzle-piece')
								])),
						pane: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$pane,
							paneCustom,
							_List_fromArray(
								[
									A2(
									elm$html$Html$h2,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('Projects')
										])),
									author$project$Main$projectsSection(model.page)
								]))
					}),
					rundis$elm_bootstrap$Bootstrap$Tab$item(
					{
						id: 'Achievements',
						link: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$link,
							_List_fromArray(
								[
									author$project$Styles$color(author$project$Styles$grey),
									author$project$Styles$fontSize('1.85em'),
									author$project$Styles$fontFamily('monospace')
								]),
							_List_fromArray(
								[
									elm$html$Html$text('λ x → x')
								])),
						pane: A2(
							rundis$elm_bootstrap$Bootstrap$Tab$pane,
							paneCustom,
							_List_fromArray(
								[
									A2(
									elm$html$Html$h2,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('Identity')
										])),
									A2(
									elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[author$project$Main$lambda]))
								]))
					})
				]),
			rundis$elm_bootstrap$Bootstrap$Tab$fill(
				rundis$elm_bootstrap$Bootstrap$Tab$withAnimation(
					rundis$elm_bootstrap$Bootstrap$Tab$config(author$project$Main$TabMsg)))));
	return A2(
		rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_fromArray(
			[
				author$project$Styles$minHeight('80vh')
			]),
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_fromArray(
					[
						rundis$elm_bootstrap$Bootstrap$Grid$Row$bottomXs,
						rundis$elm_bootstrap$Bootstrap$Grid$Row$attrs(
						_List_fromArray(
							[
								author$project$Styles$marginTop('4%')
							]))
					]),
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[tabViews]))
							]))
					]))
			]));
};
var author$project$Main$aboutView = function (model) {
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				author$project$Main$carouselAboutPage(model),
				author$project$Main$tabsAboutPage(model)
			]));
};
var elm$html$Html$iframe = _VirtualDom_node('iframe');
var author$project$Main$interests = A2(
	elm$html$Html$iframe,
	_List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'display', 'inline'),
			author$project$Styles$minWidth('100%'),
			author$project$Styles$minHeight('200vh'),
			elm$html$Html$Attributes$src('assets/content/Interests.html')
		]),
	_List_Nil);
var author$project$Main$pageTitle = F2(
	function (isTitle, page) {
		switch (page.$) {
			case 'About':
				return isTitle ? 'Lucas Dutton' : 'About';
			case 'Interests':
				return 'Interests';
			case 'ReadMenu':
				return 'Reads';
			case 'ReadPage':
				var content = page.a;
				return content;
			case 'Home':
				return '';
			default:
				return '404 Not Found';
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$Brand = function (a) {
	return {$: 'Brand', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$Config = function (a) {
	return {$: 'Config', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig = F2(
	function (mapper, _n0) {
		var conf = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Navbar$Config(
			mapper(conf));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$brand = F3(
	function (attributes, children, config_) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig,
			function (conf) {
				return _Utils_update(
					conf,
					{
						brand: elm$core$Maybe$Just(
							rundis$elm_bootstrap$Bootstrap$Navbar$Brand(
								A2(
									elm$html$Html$a,
									_Utils_ap(
										_List_fromArray(
											[
												elm$html$Html$Attributes$class('navbar-brand')
											]),
										attributes),
									children)))
					});
			},
			config_);
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$Light = {$: 'Light'};
var rundis$elm_bootstrap$Bootstrap$Navbar$Roled = function (a) {
	return {$: 'Roled', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$config = function (toMsg) {
	return rundis$elm_bootstrap$Bootstrap$Navbar$Config(
		{
			brand: elm$core$Maybe$Nothing,
			customItems: _List_Nil,
			items: _List_Nil,
			options: {
				attributes: _List_Nil,
				fix: elm$core$Maybe$Nothing,
				isContainer: false,
				scheme: elm$core$Maybe$Just(
					{
						bgColor: rundis$elm_bootstrap$Bootstrap$Navbar$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Role$Light),
						modifier: rundis$elm_bootstrap$Bootstrap$Navbar$Light
					}),
				toggleAt: rundis$elm_bootstrap$Bootstrap$General$Internal$XS
			},
			toMsg: toMsg,
			withAnimation: false
		});
};
var rundis$elm_bootstrap$Bootstrap$Navbar$Dark = {$: 'Dark'};
var rundis$elm_bootstrap$Bootstrap$Navbar$updateOptions = F2(
	function (mapper, _n0) {
		var conf = _n0.a;
		return rundis$elm_bootstrap$Bootstrap$Navbar$Config(
			_Utils_update(
				conf,
				{
					options: mapper(conf.options)
				}));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$scheme = F3(
	function (modifier, bgColor, conf) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$updateOptions,
			function (opt) {
				return _Utils_update(
					opt,
					{
						scheme: elm$core$Maybe$Just(
							{bgColor: bgColor, modifier: modifier})
					});
			},
			conf);
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$dark = A2(
	rundis$elm_bootstrap$Bootstrap$Navbar$scheme,
	rundis$elm_bootstrap$Bootstrap$Navbar$Dark,
	rundis$elm_bootstrap$Bootstrap$Navbar$Roled(rundis$elm_bootstrap$Bootstrap$Internal$Role$Dark));
var rundis$elm_bootstrap$Bootstrap$Navbar$Top = {$: 'Top'};
var rundis$elm_bootstrap$Bootstrap$Navbar$fixTop = function (conf) {
	return A2(
		rundis$elm_bootstrap$Bootstrap$Navbar$updateOptions,
		function (opts) {
			return _Utils_update(
				opts,
				{
					fix: elm$core$Maybe$Just(rundis$elm_bootstrap$Bootstrap$Navbar$Top)
				});
		},
		conf);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$Item = function (a) {
	return {$: 'Item', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Navbar$itemLink = F2(
	function (attributes, children) {
		return rundis$elm_bootstrap$Bootstrap$Navbar$Item(
			{attributes: attributes, children: children});
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$itemLinkActive = function (attributes) {
	return rundis$elm_bootstrap$Bootstrap$Navbar$itemLink(
		A2(
			elm$core$List$cons,
			elm$html$Html$Attributes$class('active'),
			attributes));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$items = F2(
	function (items_, config_) {
		return A2(
			rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig,
			function (conf) {
				return _Utils_update(
					conf,
					{items: items_});
			},
			config_);
	});
var elm$html$Html$nav = _VirtualDom_node('nav');
var rundis$elm_bootstrap$Bootstrap$Navbar$maybeBrand = function (brand_) {
	if (brand_.$ === 'Just') {
		var b = brand_.a.a;
		return _List_fromArray(
			[b]);
	} else {
		return _List_Nil;
	}
};
var rundis$elm_bootstrap$Bootstrap$Navbar$sizeToComparable = function (size) {
	switch (size.$) {
		case 'XS':
			return 1;
		case 'SM':
			return 2;
		case 'MD':
			return 3;
		case 'LG':
			return 4;
		default:
			return 5;
	}
};
var rundis$elm_bootstrap$Bootstrap$General$Internal$SM = {$: 'SM'};
var rundis$elm_bootstrap$Bootstrap$Navbar$toScreenSize = function (windowWidth) {
	return (windowWidth <= 576) ? rundis$elm_bootstrap$Bootstrap$General$Internal$XS : ((windowWidth <= 768) ? rundis$elm_bootstrap$Bootstrap$General$Internal$SM : ((windowWidth <= 992) ? rundis$elm_bootstrap$Bootstrap$General$Internal$MD : ((windowWidth <= 1200) ? rundis$elm_bootstrap$Bootstrap$General$Internal$LG : rundis$elm_bootstrap$Bootstrap$General$Internal$XL)));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu = F2(
	function (_n0, _n1) {
		var windowWidth = _n0.a.windowWidth;
		var options = _n1.options;
		var winMedia = function () {
			if (windowWidth.$ === 'Just') {
				var s = windowWidth.a;
				return rundis$elm_bootstrap$Bootstrap$Navbar$toScreenSize(s);
			} else {
				return rundis$elm_bootstrap$Bootstrap$General$Internal$XS;
			}
		}();
		return _Utils_cmp(
			rundis$elm_bootstrap$Bootstrap$Navbar$sizeToComparable(winMedia),
			rundis$elm_bootstrap$Bootstrap$Navbar$sizeToComparable(options.toggleAt)) > 0;
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$Shown = {$: 'Shown'};
var rundis$elm_bootstrap$Bootstrap$Navbar$StartDown = {$: 'StartDown'};
var rundis$elm_bootstrap$Bootstrap$Navbar$StartUp = {$: 'StartUp'};
var rundis$elm_bootstrap$Bootstrap$Navbar$visibilityTransition = F2(
	function (withAnimation_, visibility) {
		var _n0 = _Utils_Tuple2(withAnimation_, visibility);
		if (_n0.a) {
			switch (_n0.b.$) {
				case 'Hidden':
					var _n1 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$StartDown;
				case 'StartDown':
					var _n2 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingDown;
				case 'AnimatingDown':
					var _n3 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Shown;
				case 'Shown':
					var _n4 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$StartUp;
				case 'StartUp':
					var _n5 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$AnimatingUp;
				default:
					var _n6 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Hidden;
			}
		} else {
			switch (_n0.b.$) {
				case 'Hidden':
					var _n7 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Shown;
				case 'Shown':
					var _n8 = _n0.b;
					return rundis$elm_bootstrap$Bootstrap$Navbar$Hidden;
				default:
					return rundis$elm_bootstrap$Bootstrap$Navbar$Hidden;
			}
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$transitionHandler = F2(
	function (state, configRec) {
		return elm$json$Json$Decode$succeed(
			configRec.toMsg(
				A2(
					rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
					function (s) {
						return _Utils_update(
							s,
							{
								visibility: A2(rundis$elm_bootstrap$Bootstrap$Navbar$visibilityTransition, configRec.withAnimation, s.visibility)
							});
					},
					state)));
	});
var elm$core$String$fromFloat = _String_fromNumber;
var rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle = function (maybeHeight) {
	var pixelHeight = A2(
		elm$core$Maybe$withDefault,
		'0',
		A2(
			elm$core$Maybe$map,
			function (v) {
				return elm$core$String$fromFloat(v) + 'px';
			},
			maybeHeight));
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'position', 'relative'),
			A2(elm$html$Html$Attributes$style, 'height', pixelHeight),
			A2(elm$html$Html$Attributes$style, 'width', '100%'),
			A2(elm$html$Html$Attributes$style, 'overflow', 'hidden'),
			A2(elm$html$Html$Attributes$style, '-webkit-transition-timing-function', 'ease'),
			A2(elm$html$Html$Attributes$style, '-o-transition-timing-function', 'ease'),
			A2(elm$html$Html$Attributes$style, 'transition-timing-function', 'ease'),
			A2(elm$html$Html$Attributes$style, '-webkit-transition-duration', '0.35s'),
			A2(elm$html$Html$Attributes$style, '-o-transition-duration', '0.35s'),
			A2(elm$html$Html$Attributes$style, 'transition-duration', '0.35s'),
			A2(elm$html$Html$Attributes$style, '-webkit-transition-property', 'height'),
			A2(elm$html$Html$Attributes$style, '-o-transition-property', 'height'),
			A2(elm$html$Html$Attributes$style, 'transition-property', 'height')
		]);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$menuAttributes = F2(
	function (state, configRec) {
		var visibility = state.a.visibility;
		var height = state.a.height;
		var defaults = _List_fromArray(
			[
				elm$html$Html$Attributes$class('collapse navbar-collapse')
			]);
		switch (visibility.$) {
			case 'Hidden':
				if (height.$ === 'Nothing') {
					return ((!configRec.withAnimation) || A2(rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu, state, configRec)) ? defaults : _List_fromArray(
						[
							A2(elm$html$Html$Attributes$style, 'display', 'block'),
							A2(elm$html$Html$Attributes$style, 'height', '0'),
							A2(elm$html$Html$Attributes$style, 'overflow', 'hidden'),
							A2(elm$html$Html$Attributes$style, 'width', '100%')
						]);
				} else {
					return defaults;
				}
			case 'StartDown':
				return rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(elm$core$Maybe$Nothing);
			case 'AnimatingDown':
				return _Utils_ap(
					rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(height),
					_List_fromArray(
						[
							A2(
							elm$html$Html$Events$on,
							'transitionend',
							A2(rundis$elm_bootstrap$Bootstrap$Navbar$transitionHandler, state, configRec))
						]));
			case 'AnimatingUp':
				return _Utils_ap(
					rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(elm$core$Maybe$Nothing),
					_List_fromArray(
						[
							A2(
							elm$html$Html$Events$on,
							'transitionend',
							A2(rundis$elm_bootstrap$Bootstrap$Navbar$transitionHandler, state, configRec))
						]));
			case 'StartUp':
				return rundis$elm_bootstrap$Bootstrap$Navbar$transitionStyle(height);
			default:
				return _Utils_ap(
					defaults,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('show')
						]));
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$menuWrapperAttributes = F2(
	function (state, confRec) {
		var visibility = state.a.visibility;
		var height = state.a.height;
		var styleBlock = _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'display', 'block'),
				A2(elm$html$Html$Attributes$style, 'width', '100%')
			]);
		var display = function () {
			if (height.$ === 'Nothing') {
				return ((!confRec.withAnimation) || A2(rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu, state, confRec)) ? 'flex' : 'block';
			} else {
				return 'flex';
			}
		}();
		switch (visibility.$) {
			case 'Hidden':
				return _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', display),
						A2(elm$html$Html$Attributes$style, 'width', '100%')
					]);
			case 'StartDown':
				return styleBlock;
			case 'AnimatingDown':
				return styleBlock;
			case 'AnimatingUp':
				return styleBlock;
			case 'StartUp':
				return styleBlock;
			default:
				return ((!confRec.withAnimation) || A2(rundis$elm_bootstrap$Bootstrap$Navbar$shouldHideMenu, state, confRec)) ? _List_fromArray(
					[
						elm$html$Html$Attributes$class('collapse navbar-collapse show')
					]) : _List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'display', 'block')
					]);
		}
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$expandOption = function (size) {
	var toClass = function (sz) {
		return elm$html$Html$Attributes$class(
			'navbar-expand' + A2(
				elm$core$Maybe$withDefault,
				'',
				A2(
					elm$core$Maybe$map,
					function (s) {
						return '-' + s;
					},
					rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(sz))));
	};
	switch (size.$) {
		case 'XS':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$SM)
				]);
		case 'SM':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$MD)
				]);
		case 'MD':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$LG)
				]);
		case 'LG':
			return _List_fromArray(
				[
					toClass(rundis$elm_bootstrap$Bootstrap$General$Internal$XL)
				]);
		default:
			return _List_Nil;
	}
};
var rundis$elm_bootstrap$Bootstrap$Navbar$fixOption = function (fix) {
	if (fix.$ === 'Top') {
		return 'fixed-top';
	} else {
		return 'fixed-bottom';
	}
};
var elm$core$Basics$round = _Basics_round;
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var avh4$elm_color$Color$toCssString = function (_n0) {
	var r = _n0.a;
	var g = _n0.b;
	var b = _n0.c;
	var a = _n0.d;
	var roundTo = function (x) {
		return elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return elm$core$Basics$round(x * 10000) / 100;
	};
	return elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				elm$core$String$fromFloat(
				pct(r)),
				'%,',
				elm$core$String$fromFloat(
				pct(g)),
				'%,',
				elm$core$String$fromFloat(
				pct(b)),
				'%,',
				elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$backgroundColorOption = function (bgClass) {
	switch (bgClass.$) {
		case 'Roled':
			var role = bgClass.a;
			return A2(rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role);
		case 'Custom':
			var color = bgClass.a;
			return A2(
				elm$html$Html$Attributes$style,
				'background-color',
				avh4$elm_color$Color$toCssString(color));
		default:
			var classString = bgClass.a;
			return elm$html$Html$Attributes$class(classString);
	}
};
var rundis$elm_bootstrap$Bootstrap$Navbar$linkModifierClass = function (modifier) {
	return elm$html$Html$Attributes$class(
		function () {
			if (modifier.$ === 'Dark') {
				return 'navbar-dark';
			} else {
				return 'navbar-light';
			}
		}());
};
var rundis$elm_bootstrap$Bootstrap$Navbar$schemeAttributes = function (_n0) {
	var modifier = _n0.modifier;
	var bgColor = _n0.bgColor;
	return _List_fromArray(
		[
			rundis$elm_bootstrap$Bootstrap$Navbar$linkModifierClass(modifier),
			rundis$elm_bootstrap$Bootstrap$Navbar$backgroundColorOption(bgColor)
		]);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$navbarAttributes = function (options) {
	return _Utils_ap(
		_List_fromArray(
			[
				elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('navbar', true),
						_Utils_Tuple2('container', options.isContainer)
					]))
			]),
		_Utils_ap(
			rundis$elm_bootstrap$Bootstrap$Navbar$expandOption(options.toggleAt),
			_Utils_ap(
				function () {
					var _n0 = options.scheme;
					if (_n0.$ === 'Just') {
						var scheme_ = _n0.a;
						return rundis$elm_bootstrap$Bootstrap$Navbar$schemeAttributes(scheme_);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _n1 = options.fix;
						if (_n1.$ === 'Just') {
							var fix = _n1.a;
							return _List_fromArray(
								[
									elm$html$Html$Attributes$class(
									rundis$elm_bootstrap$Bootstrap$Navbar$fixOption(fix))
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.attributes))));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$renderCustom = function (items_) {
	return A2(
		elm$core$List$map,
		function (_n0) {
			var item = _n0.a;
			return item;
		},
		items_);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$getOrInitDropdownStatus = F2(
	function (id, _n0) {
		var dropdowns = _n0.a.dropdowns;
		return A2(
			elm$core$Maybe$withDefault,
			rundis$elm_bootstrap$Bootstrap$Navbar$Closed,
			A2(elm$core$Dict$get, id, dropdowns));
	});
var elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$toggleOpen = F3(
	function (state, id, _n0) {
		var toMsg = _n0.toMsg;
		var currStatus = A2(rundis$elm_bootstrap$Bootstrap$Navbar$getOrInitDropdownStatus, id, state);
		var newStatus = function () {
			switch (currStatus.$) {
				case 'Open':
					return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
				case 'ListenClicks':
					return rundis$elm_bootstrap$Bootstrap$Navbar$Closed;
				default:
					return rundis$elm_bootstrap$Bootstrap$Navbar$Open;
			}
		}();
		return toMsg(
			A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
				function (s) {
					return _Utils_update(
						s,
						{
							dropdowns: A3(elm$core$Dict$insert, id, newStatus, s.dropdowns)
						});
				},
				state));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdownToggle = F4(
	function (state, id, configRec, _n0) {
		var attributes = _n0.a.attributes;
		var children = _n0.a.children;
		return A2(
			elm$html$Html$a,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('nav-link dropdown-toggle'),
						elm$html$Html$Attributes$href('#'),
						A2(
						elm$html$Html$Events$custom,
						'click',
						elm$json$Json$Decode$succeed(
							{
								message: A3(rundis$elm_bootstrap$Bootstrap$Navbar$toggleOpen, state, id, configRec),
								preventDefault: true,
								stopPropagation: false
							}))
					]),
				attributes),
			children);
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdown = F3(
	function (state, configRec, _n0) {
		var ddRec = _n0.a;
		var needsDropup = A2(
			elm$core$Maybe$withDefault,
			false,
			A2(
				elm$core$Maybe$map,
				function (fix) {
					if (fix.$ === 'Bottom') {
						return true;
					} else {
						return false;
					}
				},
				configRec.options.fix));
		var isShown = !_Utils_eq(
			A2(rundis$elm_bootstrap$Bootstrap$Navbar$getOrInitDropdownStatus, ddRec.id, state),
			rundis$elm_bootstrap$Bootstrap$Navbar$Closed);
		return A2(
			elm$html$Html$li,
			_List_fromArray(
				[
					elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('nav-item', true),
							_Utils_Tuple2('dropdown', true),
							_Utils_Tuple2('shown', isShown),
							_Utils_Tuple2('dropup', needsDropup)
						]))
				]),
			_List_fromArray(
				[
					A4(rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdownToggle, state, ddRec.id, configRec, ddRec.toggle),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('dropdown-menu', true),
									_Utils_Tuple2('show', isShown)
								]))
						]),
					A2(
						elm$core$List$map,
						function (_n1) {
							var item = _n1.a;
							return item;
						},
						ddRec.items))
				]));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$renderItemLink = function (_n0) {
	var attributes = _n0.attributes;
	var children = _n0.children;
	return A2(
		elm$html$Html$li,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('nav-item')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_Utils_ap(
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('nav-link')
						]),
					attributes),
				children)
			]));
};
var rundis$elm_bootstrap$Bootstrap$Navbar$renderNav = F3(
	function (state, configRec, navItems) {
		return A2(
			elm$html$Html$ul,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('navbar-nav mr-auto')
				]),
			A2(
				elm$core$List$map,
				function (item) {
					if (item.$ === 'Item') {
						var item_ = item.a;
						return rundis$elm_bootstrap$Bootstrap$Navbar$renderItemLink(item_);
					} else {
						var dropdown_ = item.a;
						return A3(rundis$elm_bootstrap$Bootstrap$Navbar$renderDropdown, state, configRec, dropdown_);
					}
				},
				navItems));
	});
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$json$Json$Decode$decodeValue = _Json_run;
var elm$json$Json$Decode$fail = _Json_fail;
var elm$json$Json$Decode$float = _Json_decodeFloat;
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var elm$json$Json$Decode$string = _Json_decodeString;
var elm$json$Json$Decode$value = _Json_decodeValue;
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'parentElement', decoder);
};
var rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target = function (decoder) {
	return A2(elm$json$Json$Decode$field, 'target', decoder);
};
var rundis$elm_bootstrap$Bootstrap$Navbar$heightDecoder = function () {
	var tagDecoder = A3(
		elm$json$Json$Decode$map2,
		F2(
			function (tag, val) {
				return _Utils_Tuple2(tag, val);
			}),
		A2(elm$json$Json$Decode$field, 'tagName', elm$json$Json$Decode$string),
		elm$json$Json$Decode$value);
	var resToDec = function (res) {
		if (res.$ === 'Ok') {
			var v = res.a;
			return elm$json$Json$Decode$succeed(v);
		} else {
			var err = res.a;
			return elm$json$Json$Decode$fail(
				elm$json$Json$Decode$errorToString(err));
		}
	};
	var fromNavDec = elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				elm$json$Json$Decode$at,
				_List_fromArray(
					['childNodes', '2', 'childNodes', '0', 'offsetHeight']),
				elm$json$Json$Decode$float),
				A2(
				elm$json$Json$Decode$at,
				_List_fromArray(
					['childNodes', '1', 'childNodes', '0', 'offsetHeight']),
				elm$json$Json$Decode$float)
			]));
	var fromButtonDec = rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement(fromNavDec);
	return A2(
		elm$json$Json$Decode$andThen,
		function (_n0) {
			var tag = _n0.a;
			var val = _n0.b;
			switch (tag) {
				case 'NAV':
					return resToDec(
						A2(elm$json$Json$Decode$decodeValue, fromNavDec, val));
				case 'BUTTON':
					return resToDec(
						A2(elm$json$Json$Decode$decodeValue, fromButtonDec, val));
				default:
					return elm$json$Json$Decode$succeed(0);
			}
		},
		rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target(
			rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement(tagDecoder)));
}();
var rundis$elm_bootstrap$Bootstrap$Navbar$toggleHandler = F2(
	function (state, configRec) {
		var height = state.a.height;
		var updState = function (h) {
			return A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$mapState,
				function (s) {
					return _Utils_update(
						s,
						{
							height: elm$core$Maybe$Just(h),
							visibility: A2(rundis$elm_bootstrap$Bootstrap$Navbar$visibilityTransition, configRec.withAnimation, s.visibility)
						});
				},
				state);
		};
		return A2(
			elm$html$Html$Events$on,
			'click',
			A2(
				elm$json$Json$Decode$andThen,
				function (v) {
					return elm$json$Json$Decode$succeed(
						configRec.toMsg(
							(v > 0) ? updState(v) : updState(
								A2(elm$core$Maybe$withDefault, 0, height))));
				},
				rundis$elm_bootstrap$Bootstrap$Navbar$heightDecoder));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$view = F2(
	function (state, conf) {
		var configRec = conf.a;
		return A2(
			elm$html$Html$nav,
			rundis$elm_bootstrap$Bootstrap$Navbar$navbarAttributes(configRec.options),
			_Utils_ap(
				rundis$elm_bootstrap$Bootstrap$Navbar$maybeBrand(configRec.brand),
				_Utils_ap(
					_List_fromArray(
						[
							A2(
							elm$html$Html$button,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class(
									'navbar-toggler' + A2(
										elm$core$Maybe$withDefault,
										'',
										A2(
											elm$core$Maybe$map,
											function (_n0) {
												return ' navbar-toggler-right';
											},
											configRec.brand))),
									elm$html$Html$Attributes$type_('button'),
									A2(rundis$elm_bootstrap$Bootstrap$Navbar$toggleHandler, state, configRec)
								]),
							_List_fromArray(
								[
									A2(
									elm$html$Html$span,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('navbar-toggler-icon')
										]),
									_List_Nil)
								]))
						]),
					_List_fromArray(
						[
							A2(
							elm$html$Html$div,
							A2(rundis$elm_bootstrap$Bootstrap$Navbar$menuAttributes, state, configRec),
							_List_fromArray(
								[
									A2(
									elm$html$Html$div,
									A2(rundis$elm_bootstrap$Bootstrap$Navbar$menuWrapperAttributes, state, configRec),
									_Utils_ap(
										_List_fromArray(
											[
												A3(rundis$elm_bootstrap$Bootstrap$Navbar$renderNav, state, configRec, configRec.items)
											]),
										rundis$elm_bootstrap$Bootstrap$Navbar$renderCustom(configRec.customItems)))
								]))
						]))));
	});
var rundis$elm_bootstrap$Bootstrap$Navbar$withAnimation = function (config_) {
	return A2(
		rundis$elm_bootstrap$Bootstrap$Navbar$updateConfig,
		function (conf) {
			return _Utils_update(
				conf,
				{withAnimation: true});
		},
		config_);
};
var author$project$Main$navbar = function (model) {
	var pageItems = A2(
		elm$core$List$map,
		function (page) {
			var pageStr = A2(author$project$Main$pageTitle, false, page);
			return _Utils_eq(model.page, page) ? A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$itemLinkActive,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(pageStr)
					]),
				_List_fromArray(
					[
						elm$html$Html$text(pageStr)
					])) : A2(
				rundis$elm_bootstrap$Bootstrap$Navbar$itemLink,
				_List_fromArray(
					[
						elm$html$Html$Attributes$href(pageStr)
					]),
				_List_fromArray(
					[
						elm$html$Html$text(pageStr)
					]));
		},
		_List_fromArray(
			[author$project$Routes$About, author$project$Routes$Interests, author$project$Routes$ReadMenu]));
	return A2(
		rundis$elm_bootstrap$Bootstrap$Navbar$view,
		model.navbarState,
		rundis$elm_bootstrap$Bootstrap$Navbar$fixTop(
			rundis$elm_bootstrap$Bootstrap$Navbar$dark(
				A2(
					rundis$elm_bootstrap$Bootstrap$Navbar$items,
					pageItems,
					A3(
						rundis$elm_bootstrap$Bootstrap$Navbar$brand,
						_List_fromArray(
							[
								elm$html$Html$Attributes$href('Lucas Dutton')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Lucas Dutton')
							]),
						rundis$elm_bootstrap$Bootstrap$Navbar$withAnimation(
							rundis$elm_bootstrap$Bootstrap$Navbar$config(author$project$Main$NavbarMsg)))))));
};
var elm$html$Html$Attributes$target = elm$html$Html$Attributes$stringProperty('target');
var author$project$Styles$externalLink = F2(
	function (url, label) {
		return A2(
			elm$html$Html$a,
			_List_fromArray(
				[
					elm$html$Html$Attributes$href(url),
					elm$html$Html$Attributes$target('_blank')
				]),
			_List_fromArray(
				[
					elm$html$Html$text(label)
				]));
	});
var elm$html$Html$footer = _VirtualDom_node('footer');
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Offset2 = {$: 'Offset2'};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColOffset = function (a) {
	return {$: 'ColOffset', a: a};
};
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$Offset = F2(
	function (screenSize, offsetCount) {
		return {offsetCount: offsetCount, screenSize: screenSize};
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Internal$offset = F2(
	function (size, count) {
		return rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColOffset(
			A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$Offset, size, count));
	});
var rundis$elm_bootstrap$Bootstrap$Grid$Col$offsetMd2 = A2(rundis$elm_bootstrap$Bootstrap$Grid$Internal$offset, rundis$elm_bootstrap$Bootstrap$General$Internal$MD, rundis$elm_bootstrap$Bootstrap$Grid$Internal$Offset2);
var rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mr1 = elm$html$Html$Attributes$class('mr-1');
var author$project$Main$viewFooter = function () {
	var links = A2(
		elm$html$Html$ul,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('bd-footer-links')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						author$project$Styles$fontAwesome('github'),
						A2(
						elm$html$Html$span,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mr1]),
						_List_Nil),
						A2(author$project$Styles$externalLink, 'https://github.com/Necried', 'Github')
					])),
				A2(
				elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						author$project$Styles$fontAwesome('linkedin'),
						A2(
						elm$html$Html$span,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mr1]),
						_List_Nil),
						A2(author$project$Styles$externalLink, 'https://www.linkedin.com/in/lucas-matthew-dutton-50061b133/', 'LinkedIn')
					])),
				A2(
				elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						author$project$Styles$fontAwesome('hackerrank'),
						A2(
						elm$html$Html$span,
						_List_fromArray(
							[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$mr1]),
						_List_Nil),
						A2(author$project$Styles$externalLink, 'https://www.hackerrank.com/luke97', 'HackerRank')
					]))
			]));
	return A2(
		elm$html$Html$footer,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('bd-footer'),
				author$project$Styles$backgroundColor(author$project$Styles$lightGrey)
			]),
		_List_fromArray(
			[
				A2(
				rundis$elm_bootstrap$Bootstrap$Grid$containerFluid,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[
										rundis$elm_bootstrap$Bootstrap$Grid$Col$offsetMd2,
										rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs(
										_List_fromArray(
											[
												author$project$Styles$fontSize('18px')
											]))
									]),
								_List_fromArray(
									[
										elm$html$Html$text('Connect with me')
									]))
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[rundis$elm_bootstrap$Bootstrap$Grid$Col$offsetMd2]),
								_List_fromArray(
									[links]))
							])),
						A2(
						rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[rundis$elm_bootstrap$Bootstrap$Grid$Col$offsetMd2]),
								_List_fromArray(
									[
										elm$html$Html$text('Developed with Elm 0.19 and elm-bootstrap')
									]))
							]))
					]))
			]));
}();
var elm$html$Html$h6 = _VirtualDom_node('h6');
var rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$my5 = elm$html$Html$Attributes$class('my-5');
var author$project$Styles$largeBlank = A2(
	rundis$elm_bootstrap$Bootstrap$Grid$row,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			rundis$elm_bootstrap$Bootstrap$Grid$col,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$html$Html$h6,
					_List_fromArray(
						[rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$my5]),
					_List_fromArray(
						[
							elm$html$Html$text(' ')
						]))
				]))
		]));
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$node = elm$virtual_dom$VirtualDom$node;
var elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var rundis$elm_bootstrap$Bootstrap$CDN$fontAwesome = A3(
	elm$html$Html$node,
	'link',
	_List_fromArray(
		[
			elm$html$Html$Attributes$rel('stylesheet'),
			elm$html$Html$Attributes$href('https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css')
		]),
	_List_Nil);
var author$project$Main$view = function (model) {
	var title = A2(author$project$Main$pageTitle, true, model.page);
	var nav = author$project$Main$navbar(model);
	var content = function () {
		var _n0 = model.page;
		if (_n0.$ === 'Interests') {
			return author$project$Main$interests;
		} else {
			return author$project$Main$aboutView(model);
		}
	}();
	var layout = A2(
		rundis$elm_bootstrap$Bootstrap$Grid$containerFluid,
		_List_Nil,
		_List_fromArray(
			[content, author$project$Styles$largeBlank]));
	return {
		body: _List_fromArray(
			[rundis$elm_bootstrap$Bootstrap$CDN$fontAwesome, nav, layout, author$project$Main$viewFooter]),
		title: title
	};
};
var elm$browser$Browser$application = _Browser_application;
var author$project$Main$main = elm$browser$Browser$application(
	{init: author$project$Main$init, onUrlChange: author$project$Main$UrlChanged, onUrlRequest: author$project$Main$LinkClicked, subscriptions: author$project$Main$subscriptions, update: author$project$Main$update, view: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));