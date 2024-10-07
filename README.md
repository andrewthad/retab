# Retab

## Behavior

Convert indentation (a sequence of tabs at the beginning of each line) to
the symbols « (U+00AB) and » (U+00BB). The results is encoded with
ISO 8859-1 (latin1), so to view the examples in a terminal, it is probably
necessary to convert the encoding with `iconv`. The command dumps them to
stdout:

    iconv --from-code latin1 example/001/output.txt

Consider the example from `example/003/input.txt` (to be consistent with
markdown conventions, tabs are converted to spaces in this inline
presentation):

    function bar(x):
        keepGoing = true
        while keepGoing:
            result = promptUser()
            keepGoing = interpretResult(result)
    interpretResult(r):
        if(r ~ "success"):
            return true
        else:
            return false

The output does not include any tabs. Instead, U+00AB and U+00BB indicate
where the increases and decreases in indentation are:

    function bar(x):
    «
    keepGoing = true
    while keepGoing:
    «
    result = promptUser()
    keepGoing = interpretResult(result)
    »
    »
    interpretResult(r):
    «
    if(r ~ "success"):
    «
    return true
    »
    else:
    «
    return false
    »
    »

## Purpose

This exists to help parse certain whitespace-sensitive languages. This
transformation needs to happen before tokenization.

## Alternative Designs

It is possible to support leading space characters instead of tabs,
but it is more complicated since there is more to keep track of.

## Known Issues

This currently crashes if the input does not end with a newline. This
should not be hard to fix, but I have not needed to fix it yet.
