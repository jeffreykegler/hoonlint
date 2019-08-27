::
::::  /hoon/http/lib
  ::
  ::
  ::
/?    310
::
=,  mimes:html
=,  html
|%
++  request
  $:  domain/(list cord)  
      end-point/path
      req-type/$?($get {$post p/json})  
      headers/math:eyre
      queries/quay:eyre
  ==
++  send
  |=  {ost/bone pour-path/wire params/request}
  :^  ost  %them  pour-path
  `(unit hiss:eyre)`[~ (request-to-hiss params)]
::
++  request-to-hiss
  |=  request  ^-  hiss:eyre
  =-  ~&  hiss=-  -
  :-  ^-  parsed-url/purl:eyre
      :+  :+  security=%.y
            port=~
          host=[%.y [path=domain]]
        endpoint=[extensions=~ point=end-point]       ::  ++pork,
      q-strings=queries                               ::  ++quay
  ?@  req-type
    [%get headers ~]
  [%post headers ~ (as-octt:mimes:html (en-json p.req-type))]
--
