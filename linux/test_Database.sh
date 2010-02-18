cd ../src
ghc -e ":load Database"\
    -e "quickCheck prop_readWrite"\
    -e "quickCheck prop_mergeEmpty"\
    -e "quickCheck prop_mergeReflexive"\
    -e "quickCheck prop_mergeAdd"\
    -e "quickCheck prop_shuffle"\
    -e "quickCheck prop_shuffleUgly"\
    -e "quickCheck prop_shufflePretty"\
    -e "runTestTT test_blank"\
    -e "runTestTT test_combine"\
    -e "runTestTT test_preSpaces"\
    -e "runTestTT test_preTag"\
    -e "runTestTT test_unclosed"\
    -e "runTestTT test_merged"
