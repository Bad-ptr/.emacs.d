# Emacs config  
Just another Emacs config. One thing that you must do to use it -- open my-private.example(it will be opened automatically if you run emacs for the first time with this config) for edit, fill it with your info and save it with .el extension.  
Or just `cd ~/; git clone https://github.com/Bad-ptr/.emacs.d.git;` and run emacs.  

# Troubles

## Debugging  
Couse main initial function is running on timer it's not always prints all messages and fire all errors even when emacs started with --debug-init.
So it sometimes helpful to run init function manually: `M-: (my/-do-init)`.  

## Compatibility

### package.el for older emacs(< 24)  
Read here https://github.com/technomancy/package.el.  

## Other  
Also old Emacses erroring on my wrap-with.el . Just remove it!:)  

# Materials used in building that configuration  
http://milkbox.net/note/single-file-master-emacs-configuration/  
And many other articles on the internet I don't remember), sorry guys.  

# Other interesting links  
http://emacs.sexy/

http://www.emacswiki.org/

http://tuhdo.github.io/index.html

http://www.masteringemacs.org/

http://ergoemacs.org/emacs/emacs_editing_lisp.html

https://plus.google.com/u/0/communities/114815898697665598016

http://bzg.fr/emacs-strip-tease.html

http://emacsredux.com/blog/2014/01/01/a-peek-at-emacs-24-dot-4-rectangular-selection/

http://www.skybert.net/emacs/java/

http://a-nickels-worth.blogspot.co.uk/2007/11/effective-emacs.html
