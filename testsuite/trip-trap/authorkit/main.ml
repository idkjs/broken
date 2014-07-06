(* Main -- Author kit

Author: Michael Grünewald
Date: Tue Jun  3 11:45:45 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Unicode

module Test_WHTML =
struct
  open WHTML

  let document () =
    html [
      body [
	h1 (u"My first heading");
	p [c"My first paragraph."];
      ]
    ]

  let main () =
    WSGML.print (document())
end

module Test_WSGML =
struct
  open WSGML

  let document () =
    let h1 = element ~block:false "h1" [ pcdata(u"My first heading.") ] in
    let p = element ~block:false "p" [ pcdata(u"My first paragraph.") ] in
    let body = element ~block:true "body" [ h1; p] in
    let html = element ~block:true "html" [ body] in
    make
      ~declaration:"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      ~dtd:"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">"
      [ html ]

  let main () =
    WSGML.print (document());
    Format.print_flush()

end

let () = Test_WSGML.main ()
