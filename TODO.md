* Trampoline, otherwise you will run out of stack space.
* s/make-frame/bind-bindings/. The whole frame idea is kind of dumb. Really
  what you have is an environment which implements shadowing in some way.
* Make custom exceptions so that you aren't throwing random Java exceptions all
  over the place.
