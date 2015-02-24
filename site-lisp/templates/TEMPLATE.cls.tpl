%%% TEMPLATE.cls.tpl --- (>>>Short_description<<<)

%% Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
%%
%% Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
%% Date: (>>>VC_DATE<<<)
%% License: (>>>LICENSE<<<)

%%% Commentary:

%% (>>>Long_description<<<)

%% Put this file into your TEXINPUTS.  A typical document looks like:
%%   \documentclass[OPTIONS]{(>>>FILE_SANS<<<)}
%%   \begin{document}
%%   (>>>1<<<)
%%   \end{document}

%% OPTIONS defaults to (>>>2<<<)

%%% Code:

\NeedsTeXFormat{LaTeX2e}(>>>POINT<<<)[1995/12/01]
\def\@rcs@ $#1Date: #2 #3$$#4Revision: #5$ {
   \ProvidesPackage{(>>>FILE_SANS<<<)}[#2 v#5(>>>COMMENT<<<)]}
\@rcs@ $Date: (>>>VC_DATE<<<) $$Revision: 0.0 $

\DeclareOption{(>>>3<<<)}{%%%
  }

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ExecuteOptions{(>>>4<<<)}
\ProcessOptions
\LoadClass[a4paper]{article}



%%%%##########################################################################
%%%%  Main code
%%%%##########################################################################

(>>>5<<<)


\endinput

%%% Local Variables:
%%% TeX-auto-save: nil
%%% TeX-auto-parse-length: 99999
%%% End:
