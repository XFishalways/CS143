(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)


class List {

    item: String;
	next: List;

    init (s : String, n : List) : List {
        {
            item <- s;
            next <- n;
            self;
        }
    };

    next () : List {
        next
    };

    item () : String {
        item
    };
};


class Stack {

    top: List;

    push (s : String) : Stack {
        {
            top <- (new List).init(s, top);
            self;
        }
    };

    pop () : Stack {
        let
            nil : List
        in
        {
            if (isvoid top) then
               self
            else 
            {
               top <- top.next();
               self;
            }
            fi;
        }
    };

    top () : List {
        top
    };
};

class Dcmd {

    io : IO <- new IO;
    cur : List;

    display (s : Stack) : Object {
        {
            cur <- s.top();
            while (not isvoid cur) loop {
                io.out_string(cur.item().concat("\n"));
                cur <- cur.next();
            }
            pool;
            self;
        }
    };
};

class Ecmd {

    io : IO <- new IO;
    top : List;
    top1 : String;
    top2 : String;
    top3 : String;
    conv : A2I <- new A2I;
    
    calc (s : Stack) : Object {
        {
            top <- s.top();
            if (isvoid top) then
                self
            else
                {
                    top1 <- s.top().item();
                    if (top1 = "+") then
                    {
                        s.pop();
                        top2 <- s.top().item();
                        s.pop();
                        top3 <- s.top().item();
                        s.pop();
                        s.push(conv.i2a(conv.a2i(top2) + conv.a2i(top3)));
                    }
                    else 
                        if (top1 = "s") then 
                            {
                                s.pop();
                                top2 <- s.top().item();
                                s.pop();
                                top3 <- s.top().item();
                                s.pop();
                                s.push(top2);
                                s.push(top3);
                            }
                        else 
                            self
                        fi
                    fi;
                }
            fi;
        }
    };
};


class Main inherits IO {

    stack: Stack <- new Stack;
    flag : Bool;
    cmd : String;

    main() : Object {
        {
            flag <- true;
            while(flag) loop {
                out_string("<");
                cmd <- in_string();
                if (cmd = "x") then
                    flag <- false
                else
                    if (cmd = "d") then
                        (new Dcmd).display(stack)
                    else 
                        if (cmd = "e") then 
                            (new Ecmd).calc(stack)
                        else
                            stack.push(cmd)
                        fi
                    fi       
                fi;
            } 
            pool;
        }
    };

};
