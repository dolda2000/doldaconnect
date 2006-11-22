use ExtUtils::MakeMaker;
WriteMakefile(
    NAME => "Dolcon::UI",
    LIBS => ["-ldcui"],
    VERSION_FROM => 'dcui.pm', 
);
