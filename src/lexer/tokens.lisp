(defstruct token
  (type nil :type symbol)      
  (value nil)     
  (line 1 :type integer)       
  (column 1 :type integer))    

(defparameter *keywords*
  '("stable"    
    "volatile"    
    "slow"      
    "fast"      
    "func"      
    "if"        
    "else"      
    "while"     
    "return"    
    "true"      
    "false"))

(defparameter *operators*
  '("+"   
    "-"     
    "*"   
    "/"   
    "="   
    "=="  
    "!="  
    "<"   
    ">"   
    "<="  
    ">="))

(defparameter *punctuation*
  '("(" ")" 
    "{" "}"    
    "[" "]"  
    ","      
    ";"      
    "."))
