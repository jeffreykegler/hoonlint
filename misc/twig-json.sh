# USAGE:
# - |mount /=home=  in a fake-zod
# - extract this file into it (run from directory containing zod/)
# - > curl 'http://localhost:8080/===/sur/gmail-message.twig-json'
#   "[%bccl p=~[[%bcts p=p=%to q=[%base p=[%atom p=~.p]]] [%bcts p=p=%subj q=[%base p=[%atom p=~.t]]] [%bcts p=p=%body q=[%bcsm p=[%wing p=~[%wain]]]]]]\n"
#
# This is a shell archive.  Save it in a file, remove anything before
# this line, and then unpack it by entering "sh file".  Note, it may
# create directories; files and directories will be owned by you and
# have default permissions.
#
# This archive contains:
#
#	zod/home/mar/twig-json.hoon
#	zod/home/ren/twig-json.hoon
#
echo x - zod/home/mar/twig-json.hoon
sed 's/^  //' >zod/home/mar/twig-json.hoon << 'END-of-zod/home/mar/twig-json.hoon'
  ::
  ::::  /hoon/json/tree/mar
    ::
  /?    310
    ::
  ::::  compute
    ::
  =,  mimes:html
  =,  html
  |_  jon/json
  ::
  ++  grow                                                ::  convert to
    |%
    ++  mime  [/text/json (as-octt (en-json jon))]        ::  convert to %mime
    --
  ++  grab
    |%                                                    ::  convert from
    ++  noun  json                                        ::  clam from %noun
    --
  --
END-of-zod/home/mar/twig-json.hoon
echo x - zod/home/ren/twig-json.hoon
sed 's/^  //' >zod/home/ren/twig-json.hoon << 'END-of-zod/home/ren/twig-json.hoon'
  /=   src  /hoon/
  /=   pax  /$  |=([bem=beam ^] (en-beam:format bem))
  =,  format
  ^-  json
  =/  ford-esc  (cook |=([@ @] '\0a::/') ;~(plug (just '\0a') fas))
  =.  src  :: comment ford runes
    %-  of-wain
    %+  turn  (to-wain src)
    |=  a=cord  ^+  a
    ?.  =('/' (end 3 1 a))  a
    (cat 3 '::' a)
  ::
  =/  emit-dbug  |
  =/  tol  (full (ifix [gay gay] tall:(vang emit-dbug pax)))
  ~|  (to-wain src)
  (wall:enjs (wash 0^160 >(rash src tol)<))
END-of-zod/home/ren/twig-json.hoon
exit

