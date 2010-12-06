module Main where

import System.Info
import Data.Bits
import Graphics.UI.WX
import Graphics.UI.WXCore
import Alignment
import Input
import Format

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    f <- frame [text := "Protein Secondary Structure Alignment Tool",
                style := (frameDefaultStyle .|. wxMAXIMIZE)]
    html_window <- htmlWindowCreate f 1 (Rect 0 0 100 300) 5 "Output Window"
    html_contents <- varCreate (["",""],["",""])
    let start_html = "<h2>Welcome</h2><p>Paste your PSIPRED output into the boxes, then press Align to get started.</p>"
    let working_html = "<h2>Working...</h2><p>The proteins are being aligned. Please be patient...</p>"
    
    seq1 <- textCtrl f [text := "Paste first sequence here."]
    seq2 <- textCtrl f [text := "Paste second sequence here."]
    
    align_type <- choiceCreate f 1 (rectNull) ["Global alignment", "Local alignment"] 5    
    choiceSetSelection align_type 0
    save_type <- choiceCreate f 1 (rectNull) ["Save as HTML", "Save as text"] 5
    choiceSetSelection save_type 0

    -- returns alignment tuple in the format ([structure1, structure2], [aa1, aa2])
    let get_align = do input1 <- textCtrlGetValue seq1
                       input2 <- textCtrlGetValue seq2
                       align_type_selection <- choiceGetSelection align_type
                       let local = (align_type_selection == 1)
                       let alignment = align (get_input input1) (get_input input2) local
                       return alignment
    
    -- converts an alignment tuple into HTML
    let get_align_html alignment = do 
                       let structure = fst alignment
                       let aminoseq = snd alignment
                       align_type_selection <- choiceGetSelection align_type
                       let local = (align_type_selection == 1)
                       let html = "<h3>" ++ (if local then "Local" else "Global") ++ " alignment</h3>" ++
                                  (format_alignment (structure !! 0) (structure !! 1) (aminoseq !! 0) (aminoseq !! 1))
                       return html
    
    -- updates the HTML window and stored value with HTML representation of alignment
    let update_html = do htmlWindowSetPage html_window working_html
                         wxcAppSafeYield f
                         alignment <- get_align
                         varSet html_contents alignment
                         html <- get_align_html alignment
                         htmlWindowSetPage html_window html

    -- save the alignment as a text file                       
    let save_align_text = do alignment <- varGet html_contents
                             let text = alignment_text alignment
                             maybe_file <- fileSaveDialog f True True "Save results as text" [("Text files", ["*.txt"])] "" "alignment.txt"
                             case maybe_file of
                                  Nothing   -> return ()
                                  Just file -> writeFile file text

    -- save the alignment as an HTML file                             
    let save_align_html = do alignment <- varGet html_contents
                             html <- get_align_html alignment
                             maybe_file <- fileSaveDialog f True True "Save results as HTML" [("HTML files", ["*.htm", "*.html"])] "" "alignment.html"
                             case maybe_file of
                                  Nothing   -> return ()
                                  Just file -> writeFile file html

    -- save the alignment in either HTML or text format
    let save_align = do save_type_selection <- choiceGetSelection save_type
                        let save_as_html = (save_type_selection == 0)
                        case save_as_html of
                             True  -> save_align_html
                             False -> save_align_text
    
    htmlWindowSetPage html_window start_html
    
    save <- button f [text := "Save", on command := save_align]
    submit <- button f [text := "Align", on command := do update_html
                                                          set save [enabled := True]]    
    
    set save [enabled := False]
    
    set f [
           layout := column 1 $
                     [margin 10 $ row 1 [ column 1 [
                                           floatCenter (label "Enter your sequences:"),
                                           fill (widget seq1),
                                           fill (widget seq2),
                                           floatCenter (row 1 [(widget align_type),
                                                               (widget submit)]),
                                         
                                           fill (widget html_window),
                                           floatCenter (row 1 [(widget save_type),
                                                               (widget save)])
                                           ]
                                        ]]
           ]

    frameShowFullScreen f (os == "windows" || os == "mingw32") 0
    
    return ()
