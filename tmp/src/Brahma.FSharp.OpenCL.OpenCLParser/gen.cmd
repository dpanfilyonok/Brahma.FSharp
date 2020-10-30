REM Generate lexer and parser for OpenCLTranslator
..\..\packages\YC.SDK\tools\YC.FsLex.exe ^
    Lexer.fsl ^
    --unicode ^
    -o Lexer.fsl.fs
..\..\packages\YC.SDK\tools\YC.YaccConstructor.exe ^
    -i Parser.yrd
