INCLUDE         ^[[:blank:]]*"%include"[:space:]*(.*)\n
LWS	        [[:space:]^\n]+
COMMENT         "%"([^\n]|\\\n)*\n
NAME		[^{}\[\]\(\)[:space:],;\\\d`][^{}\[\]\(\)[:space:],;\\`]*
FLOAT		\d*(\d|\.\d|\d\.)\d*
INTEGER		0|[1-9]\d*
STRING		\"(\\.|\r?\n|[^\\\n"])*\"
sSTRING          \".*[^\\]\"

VALID_CHAR	(`[^[:space:]])|(`\\[bfnrtvs])
INVALID_CHAR    (`[:space:]?)|(`\\[^bfnrtvs\\])

VALID_WORD      '{NAME}
INVALID_WORD    '[:space:]?

