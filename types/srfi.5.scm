(((name . "let")
  (signature
   syntax-rules
   ()
   ((_ ((var1 init1) ...) body))
   ((_ name ((var1 init1) ...) body))
   ((_ name ((var1 init1) ... var-rest rest-init ...) body))
   ((_ (name (var1 init1) ...) body))
   ((_ (name (var1 init1) ... var-rest rest-init ...) body)))
  (desc . "An informal specification follows.
Unnamed
(let ((<parameter> <argument>)...) 
  <body>...)

Named, non-signature-style, no rest argument
(let <name> ((<parameter> <argument>)...)
  <body>...)

Named, signature-style, no rest argument
(let (<name> (<parameter> <argument>)...)
  <body>...)

Named, non-signature-style, rest argument
(let <name> ((<parameter> <argument>)...
. (<rest-parameter> <rest-argument>...))
  <body>...)

Named, signature-style, rest argument
(let (<name> (<parameter> <argument>)...
. (<rest-parameter> <rest-argument>...))
  <body>...)

Let $lambda and $letrec be hygienic bindings for the lambda and letrec forms, respectively.
For informal syntax 1:
(($lambda (<parameter>...) <body>...) <argument>...)

For informal syntaxes 2 and 3:
($letrec ((<name> ($lambda (<parameter>...) <body>...)))
  (<name> <argument>...))

For informal syntaxes 4 and 5:
($letrec ((<name> ($lambda (<parameter>...
. <rest-parameter>) <body>...))) 
  (<name> <argument>... <rest-argument>...))")))
