# Generate lexer and parser for OpenCLTranslator
mono ../../packages/YC.SDK/tools/YC.FsLex.exe \
    Lexer.fsl \
    --unicode \
    -o Lexer.fsl.fs
mono ../../packages/YC.SDK/tools/YC.YaccConstructor.exe \
    -i Parser.yrd
