module Format where

row_length = 40
-- gets the leftmost n elements from a list
left :: [a] -> Int -> [a]
left [] n = []
left a 0 = []
left a n = head a : left (tail a) (n - 1)
-- gets everything after (left n)
right :: [a] -> Int -> [a]
right [] n = []
right a 0 = a
right a n = right (tail a) (n - 1)
-- format an alignment as text
alignment_text :: ([String], [String]) -> String
alignment_text ([a, b], [c, d]) = case (length a > row_length || length b > row_length) of
                                    True -> (concat [(left s row_length) ++ "\n"
                                                     | s <- [a,b,c,d]]) ++ "\n\n" ++
                                            alignment_text ([(right a row_length),
                                                             (right b row_length)],
                                                            [(right c row_length),
                                                             (right d row_length)])
                                    False -> a ++ "\n" ++ b ++ "\n"
-- character with appropriate font tags
font_html :: Char -> Bool -> String
font_html a False = "<font color='grey'>" ++ [a] ++ "</font>"
font_html 'E' True = "<font color='green'><b>E</b></font>"
font_html 'H' True = "<font color='blue'><b>H</b></font>"
font_html 'C' True = "<font color='red'>C</font>"
font_html _ True = "<font color='black'>-</font>"
-- single character as HTML
format_char_html a b = "\n<td width='24'><center>" ++ font_html a b ++ "</center></td>"
-- if one character is a gap, returns the other character; if mismatched, return nothing
same_or_nothing :: Char -> Char -> Char
same_or_nothing a b | a == '-'          = b
                    | b == '-'          = a
                    | a == b            = a
                    | otherwise         = ' '
image :: Char -> String
image 'H' = "helix"
image 'E' = "fold"
image _ = ""
-- returns the HTML of the image for a character, taking into account the previous/next characters
image_for_char :: Char -> Char -> Char -> String
image_for_char a p n = if continuous /= (False, False) && image a /= ""
                       then "<img src='" ++ (image a) ++ imagenum ++ ".png'/>" else ""
                            where continuous = (a == p, a == n)
                                  imagenum = case continuous of 
                                                (True, True) -> "2"
                                                (True, False) -> "3"
                                                (False, True) -> "1"
-- returns the HTML for the image above two aligned characters
img_rep :: String -> String -> Char -> String
img_rep a [] p = []
img_rep [] b p = []
img_rep a b p = "\n<td width='24'>" ++ image_for_char (same_or_nothing (head a) (head b)) p n ++ 
                "</td>" ++ img_rep (tail a) (tail b) (same_or_nothing (head a) (head b))
                    where n = case (length a > 1) of
                                True -> same_or_nothing (head $ tail $ a) (head $ tail $ b)
                                False -> ' '
-- format a sequence as HTML
format_sequence :: String -> Bool -> String
format_sequence [] color = []
format_sequence a color = format_char_html (head a) color ++ format_sequence (tail a) color
-- format multiple sequences as HTML, splitting after each (row_length) characters
format_sequences :: String -> String -> String -> String -> String
format_sequences a b c d = case (length a > row_length || length b > row_length) of
                                True -> format_sequences (left a row_length) (left b row_length) (left c row_length) (left d row_length) ++
                                        format_sequences (right a row_length) (right b row_length) (right c row_length) (right d row_length)
                                False -> "\n<table cellspacing='0' cellpadding='0'>" ++ "<tr>" ++ (img_rep a b ' ') ++ "</tr>" ++
                                             concat ["\n<tr>" ++ (format_sequence seq color) ++ "\n</tr>\n" | 
                                                     (seq, color) <- 
                                                     [(a, True), (b, True), (c, False), (d, False)]] ++
                                         "\n</table><br /><br />"
-- returns the HTML representation of an alignment
format_alignment :: String -> String -> String -> String -> String
format_alignment a b c d = format_sequences a b c d
