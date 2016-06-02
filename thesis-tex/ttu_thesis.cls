\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ttu_thesis}%
 [2013/11/02 0.1 LaTeX2e document class]
\LoadClass{article}

%\DeclareOption{a4paper}
%  {\setlength\paperheight {297mm}%
%   \setlength\paperwidth  {210mm}}

%\setlength{\textwidth}{155mm}
%\setlength{\textheight}{242mm}
%\setlength{\tabcolsep}{6pt}

%\renewcommand{\normalsize}{\fontsize{10}{12}\selectfont}

\let\@title\@empty
\let\@author\@empty
\let\@date\@empty
%\let\@faculty\@empty
\newcommand{\TTUFaculty}{}

\newcounter{chapter}
%\newcounter{section}[chapter]
%\newcounter{subsection}[section]

\newcommand\chapterStar[1]{%
  \addcontentsline{toc}{chapter}{#1}
  #1}
\newcommand\chapterNoStar[1]{%
  \stepcounter{chapter}%
  \addcontentsline{toc}{chapter}{\thechapter\nobreakspace#1}
  \thechapter\nobreakspace#1}

%\def\@evenhead{\thepage\hfil\slshape\leftmark}%
%\def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
%\let\@oddfoot\@empty
%\let\@evenfoot\@empty

%\def\@oddfoot{\hfill\thepage\hfill}

\def\setfaculty#1{
  %\edef\@faculty{\@faculty#1\par}
  \renewcommand{\TTUFaculty}{#1}
}
%\renewcommand\tableofcontents{%
%Table of Contents
% \@starttoc{toc}%
%    }

\renewcommand\maketitle{
  \pagenumbering{gobble}
  \begin{center}
    %{\@faculty}
    \TTUFaculty{}
    \vfill
    \Huge\@title
    \end{center}
    \vskip 3em%
    \begin{flushright}
    \@author
    \end{flushright}
    \vfill
    \begin{center}
    \@date
  \end{center}
  \newpage%
  \pagenumbering{Roman}
}

\newcommand\frontmatter{%
    \cleardoublepage
  %\@mainmatterfalse
  \pagenumbering{roman}}
\newcommand\mainmatter{%
    \cleardoublepage
  %\@mainmattertrue
  \pagenumbering{arabic}}
\newcommand\backmatter{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \@mainmatterfalse}


\ProcessOptions*
