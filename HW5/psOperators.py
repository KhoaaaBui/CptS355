# KHOA BUI - 11685409

from psItems import Value, ArrayValue, FunctionValue
class Operators:
    def __init__(self, scoperule):
        #stack variables
        self.opstack = []  #assuming top of the stack is the end of the list
        self.dictstack = []  #assuming top of the stack is the end of the list
        self.scope = scoperule
        #The builtin operators supported by our interpreter
        self.builtin_operators = {
             # TO-DO in part1
             # include the key value pairs where the keys are the PostScrip opertor names and the values are the function values that implement that operator. 
             # Make sure **not to call the functions** 
             "add":self.add,
             "sub":self.sub,
             "mul":self.mul,
             "mod":self.mod,
             "eq":self.eq,
             "lt":self.lt,
             "gt":self.gt,
             "length":self.length,
             "getinterval":self.getinterval,
             "putinterval":self.putinterval,
             "aload":self.aload,
             "astore":self.astore,
             "pop":self.pop,
             "stack":self.stack,
             "dup":self.dup,
             "copy":self.copy,
             "count":self.count,
             "clear":self.clear,
             "exch":self.exch,
             "roll":self.roll,
             # "dict":self.psDict,
             # "begin":self.begin,
             # "end":self.end,
             "def":self.psDef,
             "if":self.psIf,
             "ifelse":self.psIfelse,
             "forall":self.forall,
             "repeat":self.repeat
        }
    #-------  Operand Stack Operators --------------
    """
        Helper function. Pops the top value from opstack and returns it.
    """
    def opPop(self):
        # empty check
        if not self.opstack:
            print("opstack is empty")
            return None
        else:
            return self.opstack.pop()

    """
       Helper function. Pushes the given value to the opstack.
    """
    def opPush(self,value):
        self.opstack.append(value)
        
    #------- Dict Stack Operators --------------
    
    """
       Helper function. Pops the top dictionary from dictstack and returns it.
    """   
    def dictPop(self):
        if not self.dictstack:
            print("dictstack is empty")
            return None
        else:
            return self.dictstack.pop()

    """
       Helper function. Pushes the given dictionary onto the dictstack. 
    """   
    def dictPush(self,d):
        self.dictstack.append(d)

    """
       Helper function. Adds name:value pair to the top dictionary in the dictstack.
       (Note: If the dictstack is empty, first adds an empty dictionary to the dictstack then adds the name:value to that. 
    """   
    def define(self,name, value):
        # if dictstack is not empty, add value to stack
        if len(self.dictstack) > 0:
            # add to the dict top of the stack
            # self.dictstack[len(self.dictstack) - 1][0] = len(self.dictstack) - 1
            self.dictstack[len(self.dictstack) - 1][1][name] = value
            
        # no dict in dictstack    
        else:
            # add an empty dict
            self.dictPush((0, {}))
            # add item to the dict
            self.dictstack[0][1][name] = value
            
    """
       Helper function. Searches the dictstack for a variable or function and returns its value. 
       (Starts searching at the top of the dictstack; if name is not found returns None and prints an error message.
        Make sure to add '/' to the begining of the name.)
    """
    def lookup(self,name):
        # add '/' to the begining of the name
        name = '/' + name
        # check if object is static or dynamic
        if self.scope == "static":
            # perform static scoping lookup
            # search dictionaries following the index links in the tuples
            return self.staticFind(name, len(self.dictstack) - 1)
        else:
            # searching the stack from top to bottom (end of list)
            for d in list(reversed(self.dictstack)):
                # search in the dictionary
                if name in d[1]:
                    return d[1][name]
            print("variable or function does not exist in dictstack")
            return None

    def staticFind(self, name, i):
        # search the dictionary at the static index
        if name in self.dictstack[i][1]:
            return self.dictstack[i][1][name]
        # reach the start of the list
        elif i == 0:
            print("Variable or function does not exist in dictstack")
            return None
        else:
            # recursively call the next dictionary
            return self.staticFind(name, self.dictstack[i][0])
    
    #------- Arithmetic Operators --------------
    
    """
       Pops 2 values from opstack; checks if they are numerical (int); adds them; then pushes the result back to opstack. 
    """   
    def add(self):
        if len(self.opstack) > 1:
            op1 = self.opPop()
            op2 = self.opPop()
            if isinstance(op1,int) and isinstance(op2,int):
                self.opPush(op1 + op2)
            else:
                print("Error: add - one of the operands is not a number value")
                self.opPush(op2)
                self.opPush(op1)             
        else:
            print("Error: add expects 2 operands")
 
    """
       Pop 2 values from opstack; checks if they are numerical (int); subtracts them; and pushes the result back to opstack. 
    """   
    def sub(self):
        if len(self.opstack) > 1:
            op1 = self.opPop()
            op2 = self.opPop()
            if isinstance(op1,int) and isinstance(op2,int):
                self.opPush(op2 - op1)
            else:
                print("Error: sub - one of the operands is not a number value")
                self.opPush(op2)
                self.opPush(op1)             
        else:
            print("Error: sub expects 2 operands")

    """
        Pops 2 values from opstack; checks if they are numerical (int); multiplies them; and pushes the result back to opstack. 
    """    
    def mul(self):
        if len(self.opstack) > 1:
            op1 = self.opPop()
            op2 = self.opPop()
            if isinstance(op1, int) and isinstance(op2, int):
                self.opPush(op2 * op1)
            else:
                print("Error: mul - one of the operands is not a number value")
                self.opPush(op2)
                self.opPush(op1)
        else:
            print("Error: mul expects 2 operands")

    """
        Pops 2 values from stack; checks if they are int values; calculates the remainder of dividing the bottom value by the top one; 
        pushes the result back to opstack.
    """ 
    def mod(self):
        if len(self.opstack) > 1:
            op1 = self.opPop()
            op2 = self.opPop()
            if isinstance(op1,int) and isinstance(op2,int):
                self.opPush(op2 % op1)
            else:
                print("Error: mod - one of the operands is not a number value")
                self.opPush(op2)
                self.opPush(op1)             
        else:
            print("Error: mod expects 2 operands")
    #---------- Comparison Operators  -----------------
    """
       Pops the top two values from the opstack; pushes "True" is they are equal, otherwise pushes "False"
    """ 
    def eq(self):
        if len(self.opstack) > 1:
            op1 = self.opPop()
            op2 = self.opPop()
            if isinstance(op1,int) and isinstance(op2,int):
                self.opPush(op2 == op1)
            elif isinstance(op1, ArrayValue) and isinstance(op2, ArrayValue):
                self.opPush(op1 is op2)
            else:
                print("Error: eq - one of the operands is not a number value or ArrayValue")
                self.opPush(op2)
                self.opPush(op1)             
        else:
            print("Error: eq expects 2 operands")

    """
       Pops the top two values from the opstack; pushes "True" if the bottom value is less than the top value, otherwise pushes "False"
    """ 
    def lt(self):
        if len(self.opstack) > 1:
            op1 = self.opPop()
            op2 = self.opPop()
            if isinstance(op1,int) and isinstance(op2,int):
                self.opPush(op2 < op1)
            else:
                print("Error: lt - one of the operands is not a number value")
                self.opPush(op2)
                self.opPush(op1)             
        else:
            print("Error: lt expects 2 operands")

    """
       Pops the top two values from the opstack; pushes "True" if the bottom value is greater than the top value, otherwise pushes "False"
    """ 
    def gt(self):
        if len(self.opstack) > 1:
            op1 = self.opPop()
            op2 = self.opPop()
            if isinstance(op1,int) and isinstance(op2,int):
                self.opPush(op2 > op1)
            else:
                print("Error: gt - one of the operands is not a number value")
                self.opPush(op2)
                self.opPush(op1)             
        else:
            print("Error: gt expects 2 operands")

    # ------- Array Operators --------------
    """ 
       Pops an array value from the operand opstack and calculates the length of it. Pushes the length back onto the opstack.
       The `length` method should support ArrayValue values.
    """
    def length(self):
        if len(self.opstack) > 0:
            val = self.opPop()
            # if arr is a ArrayValue type
            if isinstance(val, ArrayValue):
                # push the length of the array to opstack
                arr = val.value
                self.opPush(len(arr))
            else:
                print("Error: value is not an array type")
        else:
            print("Error: opstack is empty")

    """ 
        Pops the `count` (int), an (zero-based) start `index`, and an array constant (ArrayValue) from the operand stack.  
        Pushes the slice of the array of length `count` starting at `index` onto the opstack.(i.e., from `index` to `index`+`count`) 
        If the end index of the slice goes beyond the array length, will give an error. 
    """
    def getinterval(self):
        if len(self.opstack) > 2:
            count = self.opPop()
            index = self.opPop()
            val = self.opPop()
            # check if arr has the correct type
            if isinstance(val, ArrayValue):
                # check slice length with arr length
                if (count + index) > len(val.value):
                    print("Error: slice length goes beyond array length")
                # push slice to opstack
                else:
                    arr = ArrayValue(val.value[index:(index + count)])
                    self.opPush(arr)
            else:
                print("Error: value is not an array type")
        else:
            print("Error: expects 3 operands")

    """ 
        Pops an array constant (ArrayValue), start `index` (int), and another array constant (ArrayValue) from the operand stack.  
        Replaces the slice in the bottom ArrayValue starting at `index` with the top ArrayValue (the one we popped first). 
        The result is not pushed onto the stack.
        The index is 0-based. If the end index of the slice goes beyond the array length, will give an error. 
    """
    def putinterval(self):
        if len(self.opstack) > 2:
            val1 = self.opPop()
            index = self.opPop()
            val2 = self.opPop()
            # check if both arr has the correct type
            if isinstance(val1, ArrayValue) and isinstance(val2, ArrayValue):
                #arr1 = val1.value
                #arr2 = val2.value
                # check for index limit
                if index + len(val1.value) > len(val2.value):
                    print("Error: index exceeding length")
                else:
                    # replace bottom arr starting at index with top arr
                    val2.value[index:(index + len(val1.value))] = val1.value
            else:
                print("Error: value is not an array type")
        else:
            print("Error: expects 3 operands")
            

    """ 
        Pops an array constant (ArrayValue) from the operand stack.  
        Pushes all values in the array constant to the opstack in order (the first value in the array should be pushed first). 
        Pushes the orginal array value back on to the stack. 
    """
    def aload(self):
        if len(self.opstack) > 0:
            val = self.opPop()
            # check if arr has the correct type
            if isinstance(val, ArrayValue):
                arr = val.value
                # pushes all values in the array to the opstack
                for v in arr:
                    self.opPush(v)
                # pushes the original array back on to the opstack
                self.opPush(val)
            else:
                print("Error: value is not an array type")
        else:
            print("Error: opstack is empty")
        
    """ 
        Pops an array constant (ArrayValue) from the operand stack.  
        Pops as many elements as the length of the array from the operand stack and stores them in the array constant. 
        The value which was on the top of the opstack will be the last element in the array. 
        Pushes the array value back onto the operand stack. 
    """
    def astore(self):
        if len(self.opstack) > 0:
            val = self.opPop()
            # check if arr has the correct type
            if isinstance(val, ArrayValue):
                temp = []
                # pop element and store them in temp
                for i in range(len(val.value)):
                    temp.append(self.opPop())
                # add reversed temp to the end of the array
                val.value = list(reversed(temp))
                # push the array value back onto the opstack
                self.opPush(val)
            else:
                print("Error: value is not an array type")
        else:
            print("Error: opstack is empty")

    #------- Stack Manipulation and Print Operators --------------

    """
       This function implements the Postscript "pop operator". Calls self.opPop() to pop the top value from the opstack and discards the value. 
    """
    def pop (self):
        if len(self.opstack) > 0:
            self.opPop()
        else:
            print("Error: opstack is empty")

    """
       Prints the opstack. The end of the list is the top of the stack. 
    """
    def stack(self):
        print("\"===**opstack**===\"")
        if len(self.opstack) > 0:
            rv = reversed(self.opstack)
            for i in rv:
                print(i)
        else:
            print("Error: opstack is empty")

        print("\"===**dictstack**===\"")
        if len(self.dictstack) > 0:
            rv = list(reversed(self.dictstack))
            # print dict index and static link index
            for i in range(0, len(rv)):
                print("{---- ", len(rv) - 1 - i," ---- ", rv[i][0] ," ----}")
                # print all elements in the dictionary
                for key, value in rv[i][1].items():
                    print(key, "\t", value)
        else:
            print("Error: dictstack is empty")
            


    """
       Copies the top element in opstack.
    """
    def dup(self):
        if len(self.opstack) > 0:
            top = self.opPop()
            self.opPush(top)
            self.opPush(top)
        else:
            print("Error: opstack is empty")

    """
       Pops an integer count from opstack, copies count number of values in the opstack. 
    """
    def copy(self):
        if len(self.opstack) > 0:
            count = self.opPop()
            copies = []
            for i in range(count):
                # append value from the top of opstack
                copies.append(self.opstack[len(self.opstack) - 1 -i])
            # copy value from copies to the opstack
            for i in reversed(copies):
                self.opPush(i)
        else:
            print("Error: opstack is empty")

    """
        Counts the number of elements in the opstack and pushes the count onto the top of the opstack.
    """
    def count(self):
        self.opPush(len(self.opstack))

    """
       Clears the opstack.
    """
    def clear(self):
        if len(self.opstack) > 0:
            self.opstack.clear()
        else:
            print("opstack is already empty")
        
    """
       swaps the top two elements in opstack
    """
    def exch(self):
        if len(self.opstack) > 1:
            top = self.opPop()
            bottom = self.opPop()
            self.opPush(top)
            self.opPush(bottom)
        else:
            print("Error: exch expects 2 operands")

    """
        Implements roll operator.
        Pops two integer values (m, n) from opstack; 
        Rolls the top m values in opstack n times (if n is positive roll clockwise, otherwise roll counter-clockwise)
    """
    def roll(self):
        if len(self.opstack) > 1:
            n = self.opPop()
            m = self.opPop()
            if n >= 0:
                # roll n times
                for i in range(n):
                    # move the top value on the stack into the m-th position from the top
                    self.opstack.insert(len(self.opstack) - m, self.opPop())
            else:
                # roll n times
                n = -n
                for i in range(n):
                    # get the value at m-th position from top of the stack
                    m_val = self.opstack[len(self.opstack) - m]
                    # remove the m-th value
                    del self.opstack[len(self.opstack) - m]
                    # push it to the top of the stack
                    self.opPush(m_val)
        else:
            print("Error: exch expects 2 operands")

    """
       Pops an integer from the opstack (size argument) and pushes an  empty dictionary onto the opstack.
    """
    def psDict(self):
        if len(self.opstack) > 0:
            # pop size argument
            self.opPop()
            # push empty dict onto opstack
            self.opPush({})
        else:
            print("Error: opstack is empty")

    """
       Pops the dictionary at the top of the opstack; pushes it to the dictstack.
    """
    def begin(self):
        if len(self.opstack) > 0:
            # pop dict at top
            self.dictPush(self.opPop())
        else:
            print("Error: opstack is empty")

    """
       Removes the top dictionary from dictstack.
    """
    def end(self):
        self.dictPop()
        
    """
       Pops a name and a value from opstack, adds the name:value pair to the top dictionary by calling define.  
    """
    def psDef(self):
        if len(self.opstack) > 1:
            value = self.opPop()
            name = self.opPop()
            self.define(name, value)
        else:
            print("Error: psDef expects 2 operands")


    # ------- if/ifelse Operators --------------
    """
       Implements if operator. 
       Pops the `ifbody` and the `condition` from opstack. 
       If the condition is True, evaluates the `ifbody`.  
    """
    def psIf(self):
        if len(self.opstack) > 1:
            ifbody = self.opPop()
            condition = self.opPop()
            if condition == True:
                # AR for the operator's block
                self.dictPush((len(self.dictstack) - 1, {}))
                ifbody.apply(self)
        else:
            print("Error: psIf expects 2 operands")

    """
       Implements ifelse operator. 
       Pops the `elsebody`, `ifbody`, and the condition from opstack. 
       If the condition is True, evaluate `ifbody`, otherwise evaluate `elsebody`. 
    """
    def psIfelse(self):
        if len(self.opstack) > 2:
            elsebody = self.opPop()
            ifbody = self.opPop()
            condition = self.opPop()
            if condition == True:
                # AR for the operator's block
                self.dictPush((len(self.dictstack) - 1, {}))
                ifbody.apply(self)
            else:
                # AR for the operator's block
                self.dictPush((len(self.dictstack) - 1, {}))
                elsebody.apply(self)
        else:
            print("Error: psIfelse expects 3 operands")

    #------- Loop Operators --------------
    """
       Implements repeat operator.   
       Pops the `loop_body` (FunctionValue) and loop `count` (int) arguments from opstack; 
       Evaluates (applies) the `loopbody` `count` times. 
       Will be completed in part-2. 
    """  
    def repeat(self):
        if len(self.opstack) > 1:
            loop_body = self.opPop()
            count = self.opPop()
            for i in range(count):
                # AR for the operator's block
                self.dictPush((len(self.dictstack) - 1, {}))
                loop_body.apply(self)
        else:
            print("Error: repeat expects 2 operands")
    """
       Implements forall operator.   
       Pops a `codearray` (FunctionValue) and an `array` (ArrayValue) from opstack; 
       Evaluates (applies) the `codearray` on every value in the `array`.  
       Will be completed in part-2. 
    """ 
    def forall(self):
        if len(self.opstack) > 1:
            codearray = self.opPop()
            array = self.opPop()
            for val in array.value:
                self.opPush(val)
                # AR for the operator's block
                self.dictPush((len(self.dictstack) - 1, {}))
                codearray.apply(self)

    #--- used in the setup of unittests 
    def clearBoth(self):
        self.opstack[:] = []
        self.dictstack[:] = []

    def cleanTop(self): 
        if len(self.opstack)>1: 
            if self.opstack[-1] is None: 
                self.opstack.pop()
