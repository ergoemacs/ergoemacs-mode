;;; ergoemacs-extras.el --- Generate Ergoemacs Extras  -*- coding: utf-8 -*-
;;; Code:


(setq ergoemacs-dir (file-name-directory
                     (or load-file-name (buffer-file-name))))

(defgroup ergoemacs-extras nil
  "Documentation and script generation"
  :group 'ergoemacs-mode)

;;; Keyboard Settings

;; SVG heavily modified from
;; http://en.wikipedia.org/wiki/File:KB_United_Kingdom.svg

;; Color scheme chose from color brewer.
(defun ergoemacs-gen-svg-quote (char)
  ;; Derived from `describe-char'
  (let* ((case-fold-search nil)
         code str)
    (save-match-data
      (cond
       ((string= char "")
        " ")
       ((string= char ">")
        "&gt;")
       ((string= char ">")
        "&lt;")
       ((string= char "\"")
        "&quot;")
       ((string-match "[A-Z0-9]" char)
        char)
       (t
        (format "&#x%04X;"
                (encode-char
                 (with-temp-buffer
                   (insert char)
                   (char-before)) 'unicode)))))))

(defun ergoemacs-get-html-key-table ()
  "Gets the key table for the current layout."
  (ergoemacs-mode -1)
  (ergoemacs-mode 1)
  (let ((case-fold-search nil)
        (shortcut-type "")
        (short-desc "")
        (standard-key-regexp
         (format "^%s\t"
                 (regexp-opt '("C-n" "C-o" "C-O" "C-S-o" "C-w" "C-s" "C-S" "C-S-s" "C-p" "C-z" "C-Z" "C-S-z"
                               "C-y"
                               "C-f" "C-a" "C-T") 'paren)))
        (tab-regexp
         (format "^%s---ergo$"
                 (regexp-opt
                  (mapcar
                   (lambda(x)
                     (format "%s" x))
                   ergoemacs-window-tab-switching) 'paren)))
        (deletion-regexp
         (format "^%s---ergo$"
                 (regexp-opt
                  (mapcar
                   (lambda(x)
                     (format "%s" x))
                   ergoemacs-deletion-functions) 'paren)))
        (movement-regexp
         (format "^%s---ergo$"
                 (regexp-opt
                  (mapcar (lambda(x)
                            (format "%s" x))
                          ergoemacs-movement-functions) 'paren)))
        (standard-shortcuts-regexp
         (regexp-opt (mapcar
                      (lambda(x) (nth 0 x))
                      (symbol-value (ergoemacs-get-fixed-layout))) 'paren))
        (ergo-shortcuts-regexp
         (regexp-opt (mapcar
                      (lambda(x) (nth 0 x))
                      (symbol-value (ergoemacs-get-variable-layout))) 'paren)))
    (with-temp-buffer
      (describe-buffer-bindings (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward (format "^%s\\(  +\\|\t\\)" standard-shortcuts-regexp) nil t)
        (end-of-line)
        (insert "---standard"))
      (goto-char (point-min))
      (while (re-search-forward standard-key-regexp nil t)
        (end-of-line)
        (insert "---cua"))
      (goto-char (point-min))
      (while (re-search-forward "\\(<apps>\\|<menu>\\)" nil t)
        (end-of-line)
        (insert "---apps"))
      (goto-char (point-min))
      (while (re-search-forward (format "^%s\\(  +\\|\t\\)" ergo-shortcuts-regexp) nil t)
        (end-of-line)
        (insert "---ergo"))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-max))
      (insert "\n")
      (goto-char (point-min))
      (while (re-search-forward "<\\([CMS]-\\)" nil t)
        (replace-match "\\1<")
        (backward-char 1))
      (goto-char (point-min))
      (while (re-search-forward "-\\([A-Z]\\)\\>" nil t)
        (unless (save-match-data (looking-at "-"))
          (replace-match (format "S-%s" (downcase (match-string 1))) t nil)))
      (goto-char (point-min))
      (while (re-search-forward ".*:.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "key[ \t]+binding[ \t]*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "-+[ \t]+-+[ \t]*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward ".*\\(Prefix Command\\|remap\\|XF86\\).*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward ".*\\(mouse\\|kp\\|mute\\|dead\\|A\\|self-insert\\|wheel\\|iso\\|/ergoemacs\\)-.*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match "\n"))
      (goto-char (point-min))
      (while (re-search-forward "^\\w+$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil t)
        (replace-match "\n"))
      (goto-char (point-min))
      (while (re-search-forward "^[^\t\n]+$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\\(  +\\|\t\\)" nil t)
         (goto-char (match-beginning 0))
        (save-restriction
          (narrow-to-region (point-at-bol) (point))
          (goto-char (point-min))
          (while (search-forward "<" nil t)
            (replace-match "<kbd>")
            (when (search-forward ">" nil t)
              (replace-match "</kbd>")))
          (goto-char (point-min))
          (while (re-search-forward "S-" nil t)
            (replace-match "<kbd>⇧Shift</kbd>+" t))
          (goto-char (point-min)) 
          (while (re-search-forward "C-" nil t)
            (replace-match "<kbd>Ctrl</kbd>+" t t))
          (goto-char (point-min))
          (while (re-search-forward "M-" nil t)
            (replace-match "<kbd>Alt</kbd>+" t t))
          (goto-char (point-min))
          (while (re-search-forward "\\<\\(.\\)\\>" nil t)
            (replace-match "<kbd>\\1</kbd>" t))
          (goto-char (point-min))
          (while (re-search-forward "[+]\\(.\\)\\( \\|$\\)" nil t)
            (replace-match "+<kbd>\\1</kbd>\\2" t))
          (goto-char (point-min))
          (while (re-search-forward " \\(.\\)\\( \\|$\\)" nil t)
            (replace-match " <kbd>\\1</kbd>\\2" t))
          (goto-char (point-min))
          (while (re-search-forward " \\([^>]\\)$" nil t)
            (replace-match " <kbd>\\1</kbd>" t))
          (goto-char (point-min))
          (insert "【")
          (goto-char (point-max))
          (insert "】"))
        (when (looking-at "[ \t]*")
          (replace-match "</td><td>"))
        (goto-char (point-at-bol))
        (insert "<tr><td>")
        (goto-char (point-at-eol))
        (insert "</td></tr>"))
      (goto-char (point-min))
      (while (re-search-forward ".*【】.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^.*<td>\\(.*\\)</td></tr>" nil t)
        (if (= 1 (length (match-string 1)))
            (replace-match "")
          (if (intern-soft (save-match-data
                             (replace-regexp-in-string
                              "---\\(standard\\(---cua\\)?\\|ergo\\|apps\\)" ""
                              (match-string 1))))
              (progn
                (setq shortcut-type "Unclassified")
                (setq short-desc "")
                (mapc
                 (lambda(x)
                   ;; (message "%s" (nth 1 x))
                   ;; (message "%s" (nth 1 (nth 0 x)))
                   (when (string= (format "%s" (nth 1 x))
                                  (save-match-data
                                    (replace-regexp-in-string
                                     "---\\(standard\\(---cua\\)?\\|ergo\\|apps\\)" "" (match-string 1))))
                     (setq shortcut-type
                           (if (save-match-data (string-match "---apps" (match-string 1)))
                               "Ergonomic Unchorded"
                             (if (save-match-data (string-match "---standard---cua" (match-string 1)))
                                 "Standard"
                               (if (save-match-data (string-match "---standard" (match-string 1)))
                                   "Fixed"
                                 (if (save-match-data (string-match "---ergo" (match-string 1)))
                                     (if (save-match-data (string-match movement-regexp (match-string 1)))
                                         "Ergonomic Movement"
                                       (if (save-match-data
                                             (string-match deletion-regexp (match-string 1)))
                                           "Ergonomic Deletion"
                                         (if (save-match-data
                                               (string-match tab-regexp (match-string 1)))
                                             "Ergonomic Window/Frame"
                                           "Ergonomic Misc")))
                                   "Duplicate")))))
                     (when (nth 2 x)
                       (setq short-desc (nth 2 x)))))
                 `(,@(symbol-value (ergoemacs-get-variable-layout))
                   ,@(symbol-value (ergoemacs-get-fixed-layout))))
                (save-restriction
                  (narrow-to-region (point-at-bol) (point-at-eol))
                  (when (re-search-backward "---\\(standard\\(---cua\\)?\\|ergo\\|apps\\)" nil t)
                    (replace-match "")))
                (goto-char (point-at-bol))
                (search-forward "<tr>" nil t)
                (insert "<td>")
                (insert shortcut-type)
                (insert "</td>")
                (when (search-forward "</td>")
                  (insert "<td>")
                  (insert short-desc)
                  (insert "</td>"))
                (goto-char (point-at-eol))
                ;; (message "%s" (match-string 1))
                )
            (replace-match ""))))
      (while (re-search-forward "\n\n+" nil t)
        (replace-match "\n"))
      
      (sort-lines nil (point-min) (point-max))
      (buffer-string))))

(defun ergoemacs-get-html-key-tables ()
  "Get key table and convert to HTML"
  (flet ((ergoemacs-unset-redundant-global-keys ()))
    (let* ((lay (ergoemacs-get-layouts))
           (saved-layout ergoemacs-keyboard-layout)
           (tbl "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <meta name=\"keywords\" content=\"\" />
    <meta name=\"description\" content=\"\" />
    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
    <title></title>
    <link rel=\"stylesheet\" href=\"../basic.css\" />
    <script language=\"javascript\" type=\"text/javascript\" src=\"../tablefilter.js\"></script>
   </head>
    <table id=\"table_keys\"><tr><th>Type</th>><th>Key</th><th>Short Desc</th><th>Emacs Function</th></tr>")
           (extra-dir)
           (curr-dir)
           (saved-theme ergoemacs-theme))
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (when (not (file-exists-p extra-dir))
        (make-directory extra-dir t))
      (mapc
       (lambda(x)
         (message "Generate Kbd table for %s" x)
         (ergoemacs-set-default 'ergoemacs-theme nil)
         (ergoemacs-set-default 'ergoemacs-keyboard-layout x)
         (ergoemacs-mode 1)
         (setq curr-dir (expand-file-name "ergo-layouts" extra-dir))
         (when (not (file-exists-p curr-dir))
           (make-directory curr-dir t))
         (with-temp-file (expand-file-name (format "ergoemacs-layout-%s.html" x) curr-dir)
           (insert tbl)
           (insert (ergoemacs-get-html-key-table))
           (insert "</table><script language=\"javascript\" type=\"text/javascript\">
  //<![CDATA[
        var table_keys_Props =      {
                                                        col_0: \"select\", on_change: true, display_all_text: \" [ Show all ] \", rows_counter: true, alternate_rows: true
                                                };
        setFilterGrid( \"table_keys\",table_keys_Props );
//]]>
</script></html>"))
         (mapc
          (lambda(y)
            (condition-case err
                (progn
                  (ergoemacs-set-default 'ergoemacs-theme y)
                  (ergoemacs-mode 1)
                  (message "\tTheme %s" y)
                  (setq curr-dir (expand-file-name y extra-dir))
                  (when (not (file-exists-p curr-dir))
                    (make-directory curr-dir t))
                  (with-temp-file (expand-file-name (format "ergoemacs-layout-%s.html" x) curr-dir)
                    (insert tbl)
                    (insert (ergoemacs-get-html-key-table))
                    (insert "</table><script language=\"javascript\" type=\"text/javascript\">
  //<![CDATA[
        var table_keys_Props =      {
                                                        col_0: \"select\"
                                                };
        setFilterGrid( \"table_keys\",table_keys_Props );
//]]>
</script></html>")))
              (error (message "Error generating theme %s; %s" y err))))
          (sort (ergoemacs-get-themes) 'string<)))
       lay)
      (message "Setting theme back to %s" saved-theme)
      (ergoemacs-set-default 'ergoemacs-theme saved-theme)
      (ergoemacs-set-default 'ergoemacs-keyboard-layout saved-layout)
      (ergoemacs-mode 1)
      t)))

(defun ergoemacs-get-html ()
  "Gets a HTML description of ErgoEmacs"
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode %s/ergoemacs-extras --eval \"(ergoemacs-get-html)\" &"
                               (ergoemacs-emacs-exe)
                               ergoemacs-dir ergoemacs-dir)))
    (if (not (file-exists-p (expand-file-name "Readme.org" ergoemacs-dir)))
        (message "Not creating HTML information file.  Readme.org was not found.")
      (let ((extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory)))
        (when (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
        (with-temp-file (expand-file-name "tablefilter.js" extra-dir)
          (insert "/*====================================================
        - HTML Table Filter Generator v1.6
        - By Max Guglielmi
        - mguglielmi.free.fr/scripts/TableFilter/?l=en
        - please do not change this comment
        - don't forget to give some credit... it's always
        good for the author
        - Special credit to Cedric Wartel and 
        cnx.claude@free.fr for contribution and 
        inspiration
=====================================================*/

// global vars
var TblId, SearchFlt, SlcArgs;
TblId = new Array(), SlcArgs = new Array();


function setFilterGrid(id)
/*====================================================
        - Checks if id exists and is a table
        - Then looks for additional params 
        - Calls fn that generates the grid
=====================================================*/
{       
        var tbl = grabEBI(id);
        var ref_row, fObj;
        if(tbl != null && tbl.nodeName.toLowerCase() == \"table\")
        {                                               
                if(arguments.length>1)
                {
                        for(var i=0; i<arguments.length; i++)
                        {
                                var argtype = typeof arguments[i];
                                
                                switch(argtype.toLowerCase())
                                {
                                        case \"number\":
                                                ref_row = arguments[i];
                                        break;
                                        case \"object\":
                                                fObj = arguments[i];
                                        break;
                                }//switch
                                                        
                        }//for
                }//if
                
                ref_row == undefined ? ref_row=2 : ref_row=(ref_row+2);
                var ncells = getCellsNb(id,ref_row);
                tbl.tf_ncells = ncells;
                if(tbl.tf_ref_row==undefined) tbl.tf_ref_row = ref_row;
                tbl.tf_Obj = fObj;
                if( !hasGrid(id) ) AddGrid(id);         
        }//if tbl!=null
}

function AddGrid(id)
/*====================================================
        - adds a row containing the filtering grid
=====================================================*/
{       
        TblId.push(id);
        var t = grabEBI(id);
        var f = t.tf_Obj, n = t.tf_ncells;      
        var inpclass, fltgrid, displayBtn, btntext, enterkey;
        var modfilter_fn, display_allText, on_slcChange;
        var displaynrows, totrows_text, btnreset, btnreset_text;
        var sort_slc, displayPaging, pagingLength, displayLoader;
        var load_text, exactMatch, alternateBgs, colOperation;
        var rowVisibility, colWidth, bindScript;
        
        f!=undefined && f[\"grid\"]==false ? fltgrid=false : fltgrid=true;//enables/disables filter grid
        f!=undefined && f[\"btn\"]==true ? displayBtn=true : displayBtn=false;//show/hides filter's validation button
        f!=undefined && f[\"btn_text\"]!=undefined ? btntext=f[\"btn_text\"] : btntext=\"go\";//defines button text
        f!=undefined && f[\"enter_key\"]==false ? enterkey=false : enterkey=true;//enables/disables enter key
        f!=undefined && f[\"mod_filter_fn\"] ? modfilter_fn=true : modfilter_fn=false;//defines alternative fn
        f!=undefined && f[\"display_all_text\"]!=undefined ? display_allText=f[\"display_all_text\"] : display_allText=\"\";//defines 1st option text
        f!=undefined && f[\"on_change\"]==false ? on_slcChange=false : on_slcChange=true;//enables/disables onChange event on combo-box 
        f!=undefined && f[\"rows_counter\"]==true ? displaynrows=true : displaynrows=false;//show/hides rows counter
        f!=undefined && f[\"rows_counter_text\"]!=undefined ? totrows_text=f[\"rows_counter_text\"] : totrows_text=\"Displayed rows: \";//defines rows counter text
        f!=undefined && f[\"btn_reset\"]==true ? btnreset=true : btnreset=false;//show/hides reset link
        f!=undefined && f[\"btn_reset_text\"]!=undefined ? btnreset_text=f[\"btn_reset_text\"] : btnreset_text=\"Reset\";//defines reset text
        f!=undefined && f[\"sort_select\"]==true ? sort_slc=true : sort_slc=false;//enables/disables select options sorting
        f!=undefined && f[\"paging\"]==true ? displayPaging=true : displayPaging=false;//enables/disables table paging
        f!=undefined && f[\"paging_length\"]!=undefined ? pagingLength=f[\"paging_length\"] : pagingLength=10;//defines table paging length
        f!=undefined && f[\"loader\"]==true ? displayLoader=true : displayLoader=false;//enables/disables loader
        f!=undefined && f[\"loader_text\"]!=undefined ? load_text=f[\"loader_text\"] : load_text=\"Loading...\";//defines loader text
        f!=undefined && f[\"exact_match\"]==true ? exactMatch=true : exactMatch=false;//enables/disbles exact match for search
        f!=undefined && f[\"alternate_rows\"]==true ? alternateBgs=true : alternateBgs=false;//enables/disbles rows alternating bg colors
        f!=undefined && f[\"col_operation\"] ? colOperation=true : colOperation=false;//enables/disbles column operation(sum,mean)
        f!=undefined && f[\"rows_always_visible\"] ? rowVisibility=true : rowVisibility=false;//makes a row always visible
        f!=undefined && f[\"col_width\"] ? colWidth=true : colWidth=false;//defines widths of columns
        f!=undefined && f[\"bind_script\"] ? bindScript=true : bindScript=false;
        
        // props are added to table in order to be easily accessible from other fns
        t.tf_fltGrid                    =       fltgrid;
        t.tf_displayBtn                 =       displayBtn;
        t.tf_btnText                    =       btntext;
        t.tf_enterKey                   =       enterkey;
        t.tf_isModfilter_fn             =       modfilter_fn;
        t.tf_display_allText    =       display_allText;
        t.tf_on_slcChange               =       on_slcChange;
        t.tf_rowsCounter                =       displaynrows;
        t.tf_rowsCounter_text   =       totrows_text;
        t.tf_btnReset                   =       btnreset;
        t.tf_btnReset_text              =       btnreset_text;
        t.tf_sortSlc                    =       sort_slc;
        t.tf_displayPaging              =       displayPaging;
        t.tf_pagingLength               =       pagingLength;
        t.tf_displayLoader              =       displayLoader;
        t.tf_loadText                   =       load_text;
        t.tf_exactMatch                 =       exactMatch;
        t.tf_alternateBgs               =       alternateBgs;
        t.tf_startPagingRow             =       0;
        
        if(modfilter_fn) t.tf_modfilter_fn = f[\"mod_filter_fn\"];// used by DetectKey fn

        if(fltgrid)
        {
                var fltrow = t.insertRow(0); //adds filter row
                fltrow.className = \"fltrow\";
                for(var i=0; i<n; i++)// this loop adds filters
                {
                        var fltcell = fltrow.insertCell(i);
                        //fltcell.noWrap = true;
                        i==n-1 && displayBtn==true ? inpclass = \"flt_s\" : inpclass = \"flt\";
                        
                        if(f==undefined || f[\"col_\"+i]==undefined || f[\"col_\"+i]==\"none\") 
                        {
                                var inptype;
                                (f==undefined || f[\"col_\"+i]==undefined) ? inptype=\"text\" : inptype=\"hidden\";//show/hide input    
                                var inp = createElm( \"input\",[\"id\",\"flt\"+i+\"_\"+id],[\"type\",inptype],[\"class\",inpclass] );                                   
                                inp.className = inpclass;// for ie<=6
                                fltcell.appendChild(inp);
                                if(enterkey) inp.onkeypress = DetectKey;
                        }
                        else if(f[\"col_\"+i]==\"select\")
                        {
                                var slc = createElm( \"select\",[\"id\",\"flt\"+i+\"_\"+id],[\"class\",inpclass] );
                                slc.className = inpclass;// for ie<=6
                                fltcell.appendChild(slc);
                                PopulateOptions(id,i);
                                if(displayPaging)//stores arguments for GroupByPage() fn
                                {
                                        var args = new Array();
                                        args.push(id); args.push(i); args.push(n);
                                        args.push(display_allText); args.push(sort_slc); args.push(displayPaging);
                                        SlcArgs.push(args);
                                }
                                if(enterkey) slc.onkeypress = DetectKey;
                                if(on_slcChange) 
                                {
                                        (!modfilter_fn) ? slc.onchange = function(){ Filter(id); } : slc.onchange = f[\"mod_filter_fn\"];
                                } 
                        }
                        
                        if(i==n-1 && displayBtn==true)// this adds button
                        {
                                var btn = createElm(
                                                                                \"input\",
                                                                                [\"id\",\"btn\"+i+\"_\"+id],[\"type\",\"button\"],
                                                                                [\"value\",btntext],[\"class\",\"btnflt\"] 
                                                                        );
                                btn.className = \"btnflt\";
                                
                                fltcell.appendChild(btn);
                                (!modfilter_fn) ? btn.onclick = function(){ Filter(id); } : btn.onclick = f[\"mod_filter_fn\"];                                 
                        }//if           
                        
                }// for i               
        }//if fltgrid

        if(displaynrows || btnreset || displayPaging || displayLoader)
        {
                
                /*** div containing rows # displayer + reset btn ***/
                var infdiv = createElm( \"div\",[\"id\",\"inf_\"+id],[\"class\",\"inf\"] );
                infdiv.className = \"inf\";// setAttribute method for class attribute doesn't seem to work on ie<=6
                t.parentNode.insertBefore(infdiv, t);
                
                if(displaynrows)
                {
                        /*** left div containing rows # displayer ***/
                        var totrows;
                        var ldiv = createElm( \"div\",[\"id\",\"ldiv_\"+id] );
                        displaynrows ? ldiv.className = \"ldiv\" : ldiv.style.display = \"none\";
                        displayPaging ? totrows = pagingLength : totrows = getRowsNb(id);
                        
                        var totrows_span = createElm( \"span\",[\"id\",\"totrows_span_\"+id],[\"class\",\"tot\"] ); // tot # of rows displayer
                        totrows_span.className = \"tot\";//for ie<=6
                        totrows_span.appendChild( createText(totrows) );
                
                        var totrows_txt = createText(totrows_text);
                        ldiv.appendChild(totrows_txt);
                        ldiv.appendChild(totrows_span);
                        infdiv.appendChild(ldiv);
                }
                
                if(displayLoader)
                {
                        /*** div containing loader  ***/
                        var loaddiv = createElm( \"div\",[\"id\",\"load_\"+id],[\"class\",\"loader\"] );
                        loaddiv.className = \"loader\";// for ie<=6
                        loaddiv.style.display = \"none\";
                        loaddiv.appendChild( createText(load_text) );   
                        infdiv.appendChild(loaddiv);
                }
                                
                if(displayPaging)
                {
                        /*** mid div containing paging displayer ***/
                        var mdiv = createElm( \"div\",[\"id\",\"mdiv_\"+id] );
                        displayPaging ? mdiv.className = \"mdiv\" : mdiv.style.display = \"none\";                                              
                        infdiv.appendChild(mdiv);
                        
                        var start_row = t.tf_ref_row;
                        var row = grabTag(t,\"tr\");
                        var nrows = row.length;
                        var npages = Math.ceil( (nrows - start_row)/pagingLength );//calculates page nb
                        
                        var slcPages = createElm( \"select\",[\"id\",\"slcPages_\"+id] );
                        slcPages.onchange = function(){
                                if(displayLoader) showLoader(id,\"\");
                                t.tf_startPagingRow = this.value;
                                GroupByPage(id);
                                if(displayLoader) showLoader(id,\"none\");
                        }
                        
                        var pgspan = createElm( \"span\",[\"id\",\"pgspan_\"+id] );
                        grabEBI(\"mdiv_\"+id).appendChild( createText(\" Page \") );
                        grabEBI(\"mdiv_\"+id).appendChild(slcPages);
                        grabEBI(\"mdiv_\"+id).appendChild( createText(\" of \") );
                        pgspan.appendChild( createText(npages+\" \") );
                        grabEBI(\"mdiv_\"+id).appendChild(pgspan);      
                        
                        for(var j=start_row; j<nrows; j++)//this sets rows to validRow=true
                        {
                                row[j].setAttribute(\"validRow\",\"true\");
                        }//for j
                        
                        setPagingInfo(id);
                        if(displayLoader) showLoader(id,\"none\");
                }
                
                if(btnreset && fltgrid)
                {
                        /*** right div containing reset button **/      
                        var rdiv = createElm( \"div\",[\"id\",\"reset_\"+id] );
                        btnreset ? rdiv.className = \"rdiv\" : rdiv.style.display = \"none\";
                        
                        var fltreset = createElm(       \"a\",
                                                                                [\"href\",\"javascript:clearFilters('\"+id+\"');Filter('\"+id+\"');\"] );
                        fltreset.appendChild(createText(btnreset_text));
                        rdiv.appendChild(fltreset);
                        infdiv.appendChild(rdiv);
                }
                
        }//if displaynrows etc.
        
        if(colWidth)
        {
                t.tf_colWidth = f[\"col_width\"];
                setColWidths(id);
        }
        
        if(alternateBgs && !displayPaging)
                setAlternateRows(id);
        
        if(colOperation)
        {
                t.tf_colOperation = f[\"col_operation\"];
                setColOperation(id);
        }
        
        if(rowVisibility)
        {
                t.tf_rowVisibility = f[\"rows_always_visible\"];
                if(displayPaging) setVisibleRows(id);
        }
        
        if(bindScript)
        {
                t.tf_bindScript = f[\"bind_script\"];
                if(     t.tf_bindScript!=undefined &&
                        t.tf_bindScript[\"target_fn\"]!=undefined )
                {//calls a fn if defined  
                        t.tf_bindScript[\"target_fn\"].call(null,id);
                }
        }//if bindScript
}

function PopulateOptions(id,cellIndex)
/*====================================================
        - populates select
        - adds only 1 occurence of a value
=====================================================*/
{
        var t = grabEBI(id);
        var ncells = t.tf_ncells, opt0txt = t.tf_display_allText;
        var sort_opts = t.tf_sortSlc, paging = t.tf_displayPaging;
        var start_row = t.tf_ref_row;
        var row = grabTag(t,\"tr\");
        var OptArray = new Array();
        var optIndex = 0; // option index
        var currOpt = new Option(opt0txt,\"\",false,false); //1st option
        grabEBI(\"flt\"+cellIndex+\"_\"+id).options[optIndex] = currOpt;
        
        for(var k=start_row; k<row.length; k++)
        {
                var cell = getChildElms(row[k]).childNodes;
                var nchilds = cell.length;
                var isPaged = row[k].getAttribute(\"paging\");
                
                if(nchilds == ncells){// checks if row has exact cell #
                        
                        for(var j=0; j<nchilds; j++)// this loop retrieves cell data
                        {
                                if(cellIndex==j)
                                {
                                        var cell_data = getCellText(cell[j]);
                                        // checks if celldata is already in array
                                        var isMatched = false;
                                        for(w in OptArray)
                                        {
                                                if( cell_data == OptArray[w] ) isMatched = true;
                                        }
                                        if(!isMatched) OptArray.push(cell_data);
                                }//if cellIndex==j
                        }//for j
                }//if
        }//for k
        
        if(sort_opts) OptArray.sort();
        for(y in OptArray)
        {
                optIndex++;
                var currOpt = new Option(OptArray[y],OptArray[y],false,false);
                grabEBI(\"flt\"+cellIndex+\"_\"+id).options[optIndex] = currOpt;                
        }
                
}

function Filter(id)
/*====================================================
        - Filtering fn
        - gets search strings from SearchFlt array
        - retrieves data from each td in every single tr
        and compares to search string for current
        column
        - tr is hidden if all search strings are not 
        found
=====================================================*/
{       
        showLoader(id,\"\");
        SearchFlt = getFilters(id);
        var t = grabEBI(id);
        t.tf_Obj!=undefined ? fprops = t.tf_Obj : fprops = new Array();
        var SearchArgs = new Array();
        var ncells = getCellsNb(id);
        var totrows = getRowsNb(id), hiddenrows = 0;
        var ematch = t.tf_exactMatch;
        var showPaging = t.tf_displayPaging;
        
        for(var i=0; i<SearchFlt.length; i++)
                SearchArgs.push( (grabEBI(SearchFlt[i]).value).toLowerCase() );
        
        var start_row = t.tf_ref_row;
        var row = grabTag(t,\"tr\");
        
        for(var k=start_row; k<row.length; k++)
        {
                /*** if table already filtered some rows are not visible ***/
                if(row[k].style.display == \"none\") row[k].style.display = \"\";
                
                var cell = getChildElms(row[k]).childNodes;
                var nchilds = cell.length;

                if(nchilds == ncells)// checks if row has exact cell #
                {
                        var cell_value = new Array();
                        var occurence = new Array();
                        var isRowValid = true;
                                
                        for(var j=0; j<nchilds; j++)// this loop retrieves cell data
                        {
                                var cell_data = getCellText(cell[j]).toLowerCase();
                                cell_value.push(cell_data);
                                
                                if(SearchArgs[j]!=\"\")
                                {
                                        var num_cell_data = parseFloat(cell_data);
                                        
                                        if(/<=/.test(SearchArgs[j]) && !isNaN(num_cell_data)) // first checks if there is an operator (<,>,<=,>=)
                                        {
                                                num_cell_data <= parseFloat(SearchArgs[j].replace(/<=/,\"\")) ? occurence[j] = true : occurence[j] = false;
                                        }
                                        
                                        else if(/>=/.test(SearchArgs[j]) && !isNaN(num_cell_data))
                                        {
                                                num_cell_data >= parseFloat(SearchArgs[j].replace(/>=/,\"\")) ? occurence[j] = true : occurence[j] = false;
                                        }
                                        
                                        else if(/</.test(SearchArgs[j]) && !isNaN(num_cell_data))
                                        {
                                                num_cell_data < parseFloat(SearchArgs[j].replace(/</,\"\")) ? occurence[j] = true : occurence[j] = false;
                                        }
                                                                                
                                        else if(/>/.test(SearchArgs[j]) && !isNaN(num_cell_data))
                                        {
                                                num_cell_data > parseFloat(SearchArgs[j].replace(/>/,\"\")) ? occurence[j] = true : occurence[j] = false;
                                        }                                       
                                        
                                        else 
                                        {                                               
                                                // Improved by Cedric Wartel (cwl)
                                                // automatic exact match for selects and special characters are now filtered
                                                // modif cwl : exact match automatique sur les select
                                                var regexp;
                                                if(ematch || fprops[\"col_\"+j]==\"select\") regexp = new RegExp('(^)'+regexpEscape(SearchArgs[j])+'($)',\"gi\");
                                                else regexp = new RegExp(regexpEscape(SearchArgs[j]),\"gi\");
                                                occurence[j] = regexp.test(cell_data);
                                        }
                                }//if SearchArgs
                        }//for j
                        
                        for(var z=0; z<ncells; z++)
                        {
                                if(SearchArgs[z]!=\"\" && !occurence[z]) isRowValid = false;
                        }//for t
                        
                }//if
                
                if(!isRowValid)
                { 
                        row[k].style.display = \"none\"; hiddenrows++; 
                        if( showPaging ) row[k].setAttribute(\"validRow\",\"false\");
                } else {
                        row[k].style.display = \"\"; 
                        if( showPaging ) row[k].setAttribute(\"validRow\",\"true\");
                }
                
        }// for k
        
        t.tf_nRows = parseInt( getRowsNb(id) )-hiddenrows;
        if( !showPaging ) applyFilterProps(id);//applies filter props after filtering process
        if( showPaging ){ t.tf_startPagingRow=0; setPagingInfo(id); }//starts paging process    
}

function setPagingInfo(id)
/*====================================================
        - Paging fn
        - calculates page # according to valid rows
        - refreshes paging select according to page #
        - Calls GroupByPage fn
=====================================================*/
{       
        var t = grabEBI(id);
        var start_row = parseInt( t.tf_ref_row );//filter start row
        var pagelength = t.tf_pagingLength;
        var row = grabTag(t,\"tr\");    
        var mdiv = grabEBI(\"mdiv_\"+id);
        var slcPages = grabEBI(\"slcPages_\"+id);
        var pgspan = grabEBI(\"pgspan_\"+id);
        var nrows = 0;
        
        for(var j=start_row; j<row.length; j++)//counts rows to be grouped 
        {
                if(row[j].getAttribute(\"validRow\") == \"true\") nrows++;
        }//for j
        
        var npg = Math.ceil( nrows/pagelength );//calculates page nb
        pgspan.innerHTML = npg; //refresh page nb span 
        slcPages.innerHTML = \"\";//select clearing shortcut
        
        if( npg>0 )
        {
                mdiv.style.visibility = \"visible\";
                for(var z=0; z<npg; z++)
                {
                        var currOpt = new Option((z+1),z*pagelength,false,false);
                        slcPages.options[z] = currOpt;
                }
        } else {/*** if no results paging select is hidden ***/
                mdiv.style.visibility = \"hidden\";
        }
        
        GroupByPage(id);
}

function GroupByPage(id)
/*====================================================
        - Paging fn
        - Displays current page rows
=====================================================*/
{
        showLoader(id,\"\");
        var t = grabEBI(id);
        var start_row = parseInt( t.tf_ref_row );//filter start row
        var pagelength = parseInt( t.tf_pagingLength );
        var paging_start_row = parseInt( t.tf_startPagingRow );//paging start row
        var paging_end_row = paging_start_row + pagelength;
        var row = grabTag(t,\"tr\");
        var nrows = 0;
        var validRows = new Array();//stores valid rows index
        
        for(var j=start_row; j<row.length; j++)
        //this loop stores valid rows index in validRows Array
        {
                var isRowValid = row[j].getAttribute(\"validRow\");
                if(isRowValid==\"true\") validRows.push(j);
        }//for j

        for(h=0; h<validRows.length; h++)
        //this loop shows valid rows of current page
        {
                if( h>=paging_start_row && h<paging_end_row )
                {
                        nrows++;
                        row[ validRows[h] ].style.display = \"\";
                } else row[ validRows[h] ].style.display = \"none\";
        }//for h
        
        t.tf_nRows = parseInt(nrows);
        applyFilterProps(id);//applies filter props after filtering process
}

function applyFilterProps(id)
/*====================================================
        - checks fns that should be called
        after filtering and/or paging process
=====================================================*/
{
        t = grabEBI(id);
        var rowsCounter = t.tf_rowsCounter;
        var nRows = t.tf_nRows;
        var rowVisibility = t.tf_rowVisibility;
        var alternateRows = t.tf_alternateBgs;
        var colOperation = t.tf_colOperation;
        
        if( rowsCounter ) showRowsCounter( id,parseInt(nRows) );//refreshes rows counter
        if( rowVisibility ) setVisibleRows(id);//shows rows always visible
        if( alternateRows ) setAlternateRows(id);//alterning row colors
        if( colOperation  ) setColOperation(id);//makes operation on a col
        showLoader(id,\"none\");
}

function hasGrid(id)
/*====================================================
        - checks if table has a filter grid
        - returns a boolean
=====================================================*/
{
        var r = false, t = grabEBI(id);
        if(t != null && t.nodeName.toLowerCase() == \"table\")
        {
                for(i in TblId)
                {
                        if(id == TblId[i]) r = true;
                }// for i
        }//if
        return r;
}

function getCellsNb(id,nrow)
/*====================================================
        - returns number of cells in a row
        - if nrow param is passed returns number of cells 
        of that specific row
=====================================================*/
{
        var t = grabEBI(id);
        var tr;
        if(nrow == undefined) tr = grabTag(t,\"tr\")[0];
        else  tr = grabTag(t,\"tr\")[nrow];
        var n = getChildElms(tr);
        return n.childNodes.length;
}

function getRowsNb(id)
/*====================================================
        - returns total nb of filterable rows starting 
        from reference row if defined
=====================================================*/
{
        var t = grabEBI(id);
        var s = t.tf_ref_row;
        var ntrs = grabTag(t,\"tr\").length;
        return parseInt(ntrs-s);
}

function getFilters(id)
/*====================================================
        - returns an array containing filters ids
        - Note that hidden filters are also returned
=====================================================*/
{
        var SearchFltId = new Array();
        var t = grabEBI(id);
        var tr = grabTag(t,\"tr\")[0];
        var enfants = tr.childNodes;
        if(t.tf_fltGrid)
        {
                for(var i=0; i<enfants.length; i++) 
                        SearchFltId.push(enfants[i].firstChild.getAttribute(\"id\"));           
        }
        return SearchFltId;
}

function clearFilters(id)
/*====================================================
        - clears grid filters
=====================================================*/
{
        SearchFlt = getFilters(id);
        for(i in SearchFlt) grabEBI(SearchFlt[i]).value = \"\";
}

function showLoader(id,p)
/*====================================================
        - displays/hides loader div
=====================================================*/
{
        var loader = grabEBI(\"load_\"+id);
        if(loader != null && p==\"none\")
                setTimeout(\"grabEBI('load_\"+id+\"').style.display = '\"+p+\"'\",150);
        else if(loader != null && p!=\"none\") loader.style.display = p;
}

function showRowsCounter(id,p)
/*====================================================
        - Shows total number of filtered rows
=====================================================*/
{
        var totrows = grabEBI(\"totrows_span_\"+id);
        if(totrows != null && totrows.nodeName.toLowerCase() == \"span\" ) 
                totrows.innerHTML = p;
}

function getChildElms(n)
/*====================================================
        - checks passed node is a ELEMENT_NODE nodeType=1
        - removes TEXT_NODE nodeType=3  
=====================================================*/
{
        if(n.nodeType == 1)
        {
                var enfants = n.childNodes;
                for(var i=0; i<enfants.length; i++)
                {
                        var child = enfants[i];
                        if(child.nodeType == 3) n.removeChild(child);
                }
                return n;       
        }
}

function getCellText(n)
/*====================================================
        - returns text + text of child nodes of a cell
=====================================================*/
{
        var s = \"\";
        var enfants = n.childNodes;
        for(var i=0; i<enfants.length; i++)
        {
                var child = enfants[i];
                if(child.nodeType == 3) s+= child.data;
                else s+= getCellText(child);
        }
        return s;
}

function getColValues(id,colindex,num)
/*====================================================
        - returns an array containing cell values of
        a column
        - needs following args:
                - filter id (string)
                - column index (number)
                - a boolean set to true if we want only 
                numbers to be returned
=====================================================*/
{
        var t = grabEBI(id);
        var row = grabTag(t,\"tr\");
        var nrows = row.length;
        var start_row = parseInt( t.tf_ref_row );//filter start row
        var ncells = getCellsNb( id,start_row );
        var colValues = new Array();
        
        for(var i=start_row; i<nrows; i++)//iterates rows
        {
                var cell = getChildElms(row[i]).childNodes;
                var nchilds = cell.length;
        
                if(nchilds == ncells)// checks if row has exact cell #
                {
                        for(var j=0; j<nchilds; j++)// this loop retrieves cell data
                        {
                                if(j==colindex && row[i].style.display==\"\" )
                                {
                                        var cell_data = getCellText( cell[j] ).toLowerCase();
                                        (num) ? colValues.push( parseFloat(cell_data) ) : colValues.push( cell_data );
                                }//if j==k
                        }//for j
                }//if nchilds == ncells
        }//for i
        return colValues;       
}

function setColWidths(id)
/*====================================================
        - sets widths of columns
=====================================================*/
{
        if( hasGrid(id) )
        {
                var t = grabEBI(id);
                t.style.tableLayout = \"fixed\";
                var colWidth = t.tf_colWidth;
                var start_row = parseInt( t.tf_ref_row );//filter start row
                var row = grabTag(t,\"tr\")[0];
                var ncells = getCellsNb(id,start_row);
                for(var i=0; i<colWidth.length; i++)
                {
                        for(var k=0; k<ncells; k++)
                        {
                                cell = row.childNodes[k];
                                if(k==i) cell.style.width = colWidth[i];
                        }//var k
                }//for i
        }//if hasGrid
}

function setVisibleRows(id)
/*====================================================
        - makes a row always visible
=====================================================*/
{
        if( hasGrid(id) )
        {
                var t = grabEBI(id);            
                var row = grabTag(t,\"tr\");
                var nrows = row.length;
                var showPaging = t.tf_displayPaging;
                var visibleRows = t.tf_rowVisibility;
                for(var i=0; i<visibleRows.length; i++)
                {
                        if(visibleRows[i]<=nrows)//row index cannot be > nrows
                        {
                                if(showPaging)
                                        row[ visibleRows[i] ].setAttribute(\"validRow\",\"true\");
                                row[ visibleRows[i] ].style.display = \"\";
                        }//if
                }//for i
        }//if hasGrid
}

function setAlternateRows(id)
/*====================================================
        - alternates row colors for better readability
=====================================================*/
{
        if( hasGrid(id) )
        {
                var t = grabEBI(id);            
                var row = grabTag(t,\"tr\");
                var nrows = row.length;
                var start_row = parseInt( t.tf_ref_row );//filter start row
                var visiblerows = new Array();
                for(var i=start_row; i<nrows; i++)//visible rows are stored in visiblerows array
                        if( row[i].style.display==\"\" ) visiblerows.push(i);
                
                for(var j=0; j<visiblerows.length; j++)//alternates bg color
                        (j % 2 == 0) ? row[ visiblerows[j] ].className = \"even\" : row[ visiblerows[j] ].className = \"odd\";
                
        }//if hasGrid
}

function setColOperation(id)
/*====================================================
        - Calculates values of a column
        - params are stored in 'colOperation' table's
        attribute
                - colOperation[\"id\"] contains ids of elements 
                showing result (array)
                - colOperation[\"col\"] contains index of 
                columns (array)
                - colOperation[\"operation\"] contains operation
                type (array, values: sum, mean)
                - colOperation[\"write_method\"] array defines 
                which method to use for displaying the 
                result (innerHTML, setValue, createTextNode).
                Note that innerHTML is the default value.
                
        !!! to be optimised
=====================================================*/
{
        if( hasGrid(id) )
        {
                var t = grabEBI(id);
                var labelId = t.tf_colOperation[\"id\"];
                var colIndex = t.tf_colOperation[\"col\"];
                var operation = t.tf_colOperation[\"operation\"];
                var outputType =  t.tf_colOperation[\"write_method\"];
                var precision = 2;//decimal precision
                
                if( (typeof labelId).toLowerCase()==\"object\" 
                        && (typeof colIndex).toLowerCase()==\"object\" 
                        && (typeof operation).toLowerCase()==\"object\" )
                {
                        var row = grabTag(t,\"tr\");
                        var nrows = row.length;
                        var start_row = parseInt( t.tf_ref_row );//filter start row
                        var ncells = getCellsNb( id,start_row );
                        var colvalues = new Array();
                                                
                        for(var k=0; k<colIndex.length; k++)//this retrieves col values
                        {
                                colvalues.push( getColValues(id,colIndex[k],true) );                    
                        }//for k
                        
                        for(var i=0; i<colvalues.length; i++)
                        {
                                var result=0, nbvalues=0;
                                for(var j=0; j<colvalues[i].length; j++ )
                                {
                                        var cvalue = colvalues[i][j];
                                        if( !isNaN(cvalue) )
                                        {
                                                switch( operation[i].toLowerCase() )
                                                {
                                                        case \"sum\":
                                                                result += parseFloat( cvalue );
                                                        break;
                                                        case \"mean\":
                                                                nbvalues++;
                                                                result += parseFloat( cvalue );
                                                        break;
                                                        //add cases for other operations
                                                }//switch
                                        }
                                }//for j
                                
                                switch( operation[i].toLowerCase() )
                                {
                                        case \"mean\":
                                                result = result/nbvalues;
                                        break;
                                }
                                
                                if(outputType != undefined && (typeof outputType).toLowerCase()==\"object\")
                                //if outputType is defined
                                {
                                        result = result.toFixed( precision );
                                        if( grabEBI( labelId[i] )!=undefined )
                                        {
                                                switch( outputType[i].toLowerCase() )
                                                {
                                                        case \"innerhtml\":
                                                                grabEBI( labelId[i] ).innerHTML = result;
                                                        break;
                                                        case \"setvalue\":
                                                                grabEBI( labelId[i] ).value = result;
                                                        break;
                                                        case \"createtextnode\":
                                                                var oldnode = grabEBI( labelId[i] ).firstChild;
                                                                var txtnode = createText( result );
                                                                grabEBI( labelId[i] ).replaceChild( txtnode,oldnode );
                                                        break;
                                                        //other cases could be added
                                                }//switch
                                        }
                                } else {
                                        try
                                        {
                                                grabEBI( labelId[i] ).innerHTML = result.toFixed( precision );
                                        } catch(e){ }//catch
                                }//else
                                
                        }//for i

                }//if typeof
        }//if hasGrid
}

function grabEBI(id)
/*====================================================
        - this is just a getElementById shortcut
=====================================================*/
{
        return document.getElementById( id );
}

function grabTag(obj,tagname)
/*====================================================
        - this is just a getElementsByTagName shortcut
=====================================================*/
{
        return obj.getElementsByTagName( tagname );
}

function regexpEscape(s)
/*====================================================
        - escapes special characters [\\^$.|?*+() 
        for regexp
        - Many thanks to Cedric Wartel for this fn
=====================================================*/
{
        // traite les caractères spéciaux [\\^$.|?*+()
        //remplace le carctère c par \\c
        function escape(e)
        {
                a = new RegExp('\\\\'+e,'g');
                s = s.replace(a,'\\\\'+e);
        }

        chars = new Array('\\\\','[','^','$','.','|','?','*','+','(',')');
        //chars.each(escape); // no prototype framework here...
        for(e in chars) escape(chars[e]);
        return s;
}

function createElm(elm)
/*====================================================
        - returns an html element with its attributes
        - accepts the following params:
                - a string defining the html element 
                to create
                - an undetermined # of arrays containing the
                couple \"attribute name\",\"value\" [\"id\",\"myId\"]
=====================================================*/
{
        var el = document.createElement( elm );         
        if(arguments.length>1)
        {
                for(var i=0; i<arguments.length; i++)
                {
                        var argtype = typeof arguments[i];
                        switch( argtype.toLowerCase() )
                        {
                                case \"object\":
                                        if( arguments[i].length==2 )
                                        {                                                       
                                                el.setAttribute( arguments[i][0],arguments[i][1] );
                                        }//if array length==2
                                break;
                        }//switch
                }//for i
        }//if args
        return el;      
}

function createText(node)
/*====================================================
        - this is just a document.createTextNode shortcut
=====================================================*/
{
        return document.createTextNode( node );
}

function DetectKey(e)
/*====================================================
        - common fn that detects return key for a given
        element (onkeypress attribute on input)
=====================================================*/
{
        var evt=(e)?e:(window.event)?window.event:null;
        if(evt)
        {
                var key=(evt.charCode)?evt.charCode:
                        ((evt.keyCode)?evt.keyCode:((evt.which)?evt.which:0));
                if(key==\"13\")
                {
                        var cid, leftstr, tblid, CallFn, Match;         
                        cid = this.getAttribute(\"id\");
                        leftstr = this.getAttribute(\"id\").split(\"_\")[0];
                        tblid = cid.substring(leftstr.length+1,cid.length);
                        t = grabEBI(tblid);
                        (t.tf_isModfilter_fn) ? t.tf_modfilter_fn.call() : Filter(tblid);
                }//if key
        }//if evt       
}

function importScript(scriptName,scriptPath)
{
        var isImported = false; 
        var scripts = grabTag(document,\"script\");

        for (var i=0; i<scripts.length; i++)
        {
                if(scripts[i].src.match(scriptPath))
                { 
                        isImported = true;      
                        break;
                }
        }

        if( !isImported )//imports script if not available
        {
                var head = grabTag(document,\"head\")[0];
                var extScript = createElm(      \"script\",
                                                                        [\"id\",scriptName],
                                                                        [\"type\",\"text/javascript\"],
                                                                        [\"src\",scriptPath]    );
                head.appendChild(extScript);
        }
}//fn importScript



/*====================================================
        - Below a collection of public functions 
        for developement purposes
        - all public methods start with prefix 'TF_'
        - These methods can be removed safely if not
        needed
=====================================================*/

function TF_GetFilterIds()
/*====================================================
        - returns an array containing filter grids ids
=====================================================*/
{
        try{ return TblId }
        catch(e){ alert('TF_GetFilterIds() fn: could not retrieve any ids'); }
}

function TF_HasGrid(id)
/*====================================================
        - checks if table has a filter grid
        - returns a boolean
=====================================================*/
{
        return hasGrid(id);
}

function TF_GetFilters(id)
/*====================================================
        - returns an array containing filters ids of a
        specified grid
=====================================================*/
{
        try
        {
                var flts = getFilters(id);
                return flts;
        } catch(e) {
                alert('TF_GetFilters() fn: table id not found');
        }
        
}

function TF_GetStartRow(id)
/*====================================================
        - returns starting row index for filtering
        process
=====================================================*/
{
        try
        {
                var t = grabEBI(id);
                return t.tf_ref_row;
        } catch(e) {
                alert('TF_GetStartRow() fn: table id not found');
        }
}

function TF_GetColValues(id,colindex,num)
/*====================================================
        - returns an array containing cell values of
        a column
        - needs following args:
                - filter id (string)
                - column index (number)
                - a boolean set to true if we want only 
                numbers to be returned
=====================================================*/
{
        if( hasGrid(id) )
        {
                return getColValues(id,colindex,num);
        }//if TF_HasGrid
        else alert('TF_GetColValues() fn: table id not found');
}

function TF_Filter(id)
/*====================================================
        - filters a table
=====================================================*/
{
        var t = grabEBI(id);
        if( TF_HasGrid(id) ) Filter(id);
        else alert('TF_Filter() fn: table id not found');
}

function TF_RemoveFilterGrid(id)
/*====================================================
        - removes a filter grid
=====================================================*/
{
        if( TF_HasGrid(id) )
        {
                var t = grabEBI(id);
                clearFilters(id);
                                
                if(grabEBI(\"inf_\"+id)!=null)
                {
                        t.parentNode.removeChild(t.previousSibling);
                }
                // remove paging here
                var row = grabTag(t,\"tr\");
                
                for(var j=0; j<row.length; j++)
                //this loop shows all rows and removes validRow attribute
                {                       
                        row[j].style.display = \"\";
                        try
                        { 
                                if( row[j].hasAttribute(\"validRow\") ) 
                                        row[j].removeAttribute(\"validRow\");
                        } //ie<=6 doesn't support hasAttribute method
                        catch(e){
                                for( var x = 0; x < row[j].attributes.length; x++ ) 
                                {
                                        if( row[j].attributes[x].nodeName.toLowerCase()==\"validrow\" ) 
                                                row[j].removeAttribute(\"validRow\");
                                }//for x
                        }//catch(e)
                }//for j                
                
                if( t.tf_alternateBgs )//removes alterning row colors
                {
                        for(var k=0; k<row.length; k++)
                        //this loop removes bg className
                        {
                                row[k].className = \"\";
                        }
                }
                
                if(t.tf_fltGrid) t.deleteRow(0);
                for(i in TblId)//removes grid id value from array
                        if(id == TblId[i]) TblId.splice(i,1);
                
        }//if TF_HasGrid
        else alert('TF_RemoveFilterGrid() fn: table id not found');
}

function TF_ClearFilters(id)
/*====================================================
        - clears grid filters only, table is not filtered
=====================================================*/
{
        if( TF_HasGrid(id) ) clearFilters(id);
        else alert('TF_ClearFilters() fn: table id not found');
}

function TF_SetFilterValue(id,index,searcharg)
/*====================================================
        - Inserts value in a specified filter
        - Params:
                - id: table id (string)
                - index: filter column index (numeric value)
                - searcharg: search string
=====================================================*/
{
        if( TF_HasGrid(id) )
        {
                var flts = getFilters(id);
                for(i in flts)
                {
                        if( i==index ) grabEBI(flts[i]).value = searcharg;
                }
        } else {
                alert('TF_SetFilterValue() fn: table id not found');
        }
}




/*====================================================
        - bind an external script fns
        - fns below do not belong to filter grid script 
        and are used to interface with external 
        autocomplete script found at the following URL:
        http://www.codeproject.com/jscript/jsactb.asp
        (credit to zichun) 
        - fns used to merge filter grid with external
        scripts
=====================================================*/
var colValues = new Array();

function setAutoComplete(id)
{
        var t = grabEBI(id);
        var bindScript = t.tf_bindScript;
        var scriptName = bindScript[\"name\"];
        var scriptPath = bindScript[\"path\"];
        initAutoComplete();
        
        function initAutoComplete()
        {
                var filters = TF_GetFilters(id);
                for(var i=0; i<filters.length; i++)
                {
                        if( grabEBI(filters[i]).nodeName.toLowerCase()==\"input\")
                        {
                                colValues.push( getColValues(id,i) );   
                        } else colValues.push( '' );
                }//for i

                try{ actb( grabEBI(filters[0]), colValues[0] ); }
                catch(e){ alert(scriptPath + \" script may not be loaded\"); }
            
        }//fn
}"))
        (with-temp-file (expand-file-name "basic.css" extra-dir)
          (insert "kbd {padding-left:.25em;padding-right:.25em;font-family:sans-serif;border:solid 1px #c2c2c2;border-radius:4px;background-color:#f0f0f0;box-shadow:1px 1px silver}

/*====================================================
  - HTML Table Filter Generator v1.6 
  elements and classes
  - edit classes below to change filter grid style
  =====================================================*/

.fltrow{ /* filter grid row appearance */
    height:20px;
    background-color:#f4f4f4;
}
.btnflt{ /* button appearance */
    font-size:11px;
    margin:0 2px 0 2px; padding:0 1px 0 1px;
    text-decoration:none; color: #fff;
    background-color:#666;
}
.flt{ /* filter (input) appearance */
    background-color:#f4f4f4; border:1px inset #ccc; 
    margin:0; width:100%;
}
.flt_s{ /* small filter (input) appearance */
    background-color:#f4f4f4; border:1px inset #ccc; 
    margin:0; width:80%;
}
.inf{ /* div containing left, middle and right divs */
    clear:both; width:auto; height:20px; 
    background:#f4f4f4; font-size:11px; 
    margin:0; padding:1px 3px 1px 3px; 
    border:1px solid #ccc;
}
.ldiv{ /* left div */
    float:left; width:30%; position:inherit; 
}
.mdiv{ /* middle div */
    float:left; width:30%; position:inherit; text-align:center; 
}
.rdiv{ /* right div */
    float:right; width:30%; position:inherit; text-align:right; 
}
.loader{ /* loader appearance */
    position:absolute; padding: 15px 0 15px 0;
    margin-top:7%; width:200px; left:40%; 
    z-index:1000; font-size:14px; font-weight:bold;
    border:1px solid #666; background:#f4f4f4; 
    text-align:center; vertical-align:middle;
}
div.mdiv select{ height:20px; }/*paging drop-down list*/
div.inf a{ color:#CC0000; }/*link appearence in .inf div*/
div.inf a:hover{ text-decoration:none; }/*link appearence in .inf div*/
.tot{ font-weight:bold; }/*rows counter*/
.even{ background-color:#fff; }/*row bg alternating color*/
.odd{ background-color:#f4f4f4; }/*row bg alternating color*/
"))
        (with-temp-file (expand-file-name "index.org" extra-dir)
          (insert-file-contents (expand-file-name "Readme.org" ergoemacs-dir))
          (goto-char (point-min))
          (when (re-search-forward "^[*]" nil t)
            (beginning-of-line)
            (insert "\n\n#+BEGIN_SRC emacs-lisp
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout \"us\")
  (require 'ergoemacs-mode)
  (ergoemacs-mode 1)
#+END_SRC
\n\n[[./ergo-layouts/ergoemacs-layout-us.png]]\n\n")))
        (find-file (expand-file-name "index.org" extra-dir))
        (execute-kbd-macro (edmacro-parse-keys "C-c C-e h"))
        (kill-buffer (current-buffer))
        (find-file (expand-file-name "index.html" extra-dir))
        (when (serach-forward "<pre class=\"example\"" nil t)
          (insert "id=\"dot_emacs\""))
        (goto-char (point-min))
        (when (search-forward "ergoemacs-layout-us.png")
          (delete-region
           (progn (re-search-backward "<" nil t)
                  (point))
           (progn (re-search-forward ">" nil t)
                  (point)))
          (insert (ergoemacs-get-html-select)))
        (save-buffer)
        (kill-buffer (current-buffer)))
      (ergoemacs-get-html-key-tables))))

(defun ergoemacs-get-html-select ()
  "Gets the list of all known layouts and the documentation associated with the layouts."
  (let ((lays (sort (ergoemacs-get-layouts) 'string<)))
    (concat
     "<script type=\"text/javascript\">
function change_layout() {
  var select = document.getElementById('select_layout');
  var selection = select.selectedIndex;
  var img = select.options[selection].value;
  select = document.getElementById('select_theme');
  selection = select.selectedIndex;
  var dir = select.options[selection].value;  
  document.getElementById('ergo_image').src = dir + \"/ergoemacs-layout-\" + img + \".png\";
  if (dir == \"kbd-layouts\"){
    dir = \"ergo-layouts\";
  } else {
    var dir2 = dir;
    if (dir2 == \"ergo-layouts\"){
       dir2 = \"nil\";
    } else {
       dir2 = '\"'+dir2+'\"';
    }
    document.getElementById('dot_emacs').innerHTML = '(setq ergoemacs-theme '+dir2+')\n(setq ergoemacs-keyboard-layout \"' + img + '\")\n(require \'ergoemacs-mode)\n(ergoemacs-mode 1)';
    }
  document.getElementById('ergo_kbd').src = dir + \"/ergoemacs-layout-\" + img + \".html\";
}
    </script><form><strong>Layout:</strong><select onchange=\"change_layout()\" id=\"select_layout\">\n"
     (mapconcat
      (lambda(lay)
        (let* ((variable (intern (concat "ergoemacs-layout-" lay)))
               (alias (condition-case nil
                          (indirect-variable variable)
                        (error variable)))
               (is-alias nil)
               (doc nil))
          (setq doc (or (documentation-property variable 'variable-documentation)
                        (progn
                          (setq is-alias t)
                          (documentation-property alias 'variable-documentation))))
          (concat "<option value=\"" lay "\"" (if (string= lay "us") " selected" "")
                  ">" doc "(" lay ")"
                  (if is-alias ", alias" "")
                  "</option>")))
      lays "\n")
     "\n</select><br/><strong>Theme:</strong><select onchange=\"change_layout()\" id=\"select_theme\"><option value=\"kbd-layouts\">Keyboard Layout</option><option value=\"ergo-layouts\" selected>Standard</option>"
     (let ((lays (sort (ergoemacs-get-themes) 'string<)))
             (mapconcat
              (lambda(lay)
                (let* ((variable (intern (concat "ergoemacs-" lay "-theme")))
                       (alias (condition-case nil
                                  (indirect-variable variable)
                                (error variable)))
                       (is-alias nil)
                       (doc nil))
                  (setq doc (or (documentation-property variable 'group-documentation)
                                (progn
                                  (setq is-alias t)
                                  (documentation-property alias 'group-documentation))))
                  (concat "<option value=\"" lay "\">" lay " - "
                          (replace-regexp-in-string
                           ">" "&lt;"
                           (replace-regexp-in-string "<" "&gt;" doc)) "</option>")))
              lays "\n"))
     "</select></form><br /><image id=\"ergo_image\" src=\"ergo-layouts/ergoemacs-layout-us.png\" /><br /><iframe id=\"ergo_kbd\" src=\"ergo-layouts/ergoemacs-layout-us.html\"
  width=\"100%\" height=\"400\"></iframe>")))

(defun ergoemacs-trans-mac-osx (key &optional swap-option-and-control)
  "Translates Emacs kbd code KEY to Mac OS X DefaultKeyBinding.dict"
  (let ((ret key)
        (case-fold-search t))
    (with-temp-buffer
      (insert ret)
      (goto-char (point-min))
      (while (re-search-forward "\\<M-" nil t)
        (replace-match (if swap-option-and-control "^" "~") nil t))
      (goto-char (point-min))
      (while (re-search-forward "\\<C-" nil t)
        (replace-match (if swap-option-and-control "~" "^") nil t))
      (setq ret (buffer-string)))
    (symbol-value 'ret)))

(defun ergoemacs-gen-mac-osx (layout &optional file-name extra swap-opiton-and-control)
  "Generates an Autohotkey Script for Ergoemacs Keybindings.
Currently only supports two modifier plus key."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "os_x_qwerty.dict.txt"))
        (xtra (or extra "os_x_opt_meta"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (i 0))
    ;; ergoemacs-variable-layout
    (if (not lay)
        (message "Layout %s not found" layout)
      (ergoemacs-setup-keys-for-layout layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      ;; Translate keys
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout ".dict. txt") extra-dir))
      (with-temp-file file
        (set-buffer-file-coding-system 'utf-8)
        (insert-file-contents (expand-file-name fn dir))
        (goto-char (point-min))
        (when (re-search-forward "QWERTY")
          (replace-match layout))
        (mapc
         (lambda(x)
           (let ((from (nth 0 x))
                 from-reg
                 (to nil))
             (setq to (ergoemacs-kbd from t))
             (if (string= from to) nil
               
               (setq from (ergoemacs-trans-mac-osx from t))
               (setq to (ergoemacs-trans-mac-osx to swap-opiton-and-control))
               (setq from-reg (regexp-quote from))
               (goto-char (point-min))
               (when (re-search-forward from-reg nil t)
                 (replace-match to t t)))))
         (symbol-value (ergoemacs-get-variable-layout)))
        (goto-char (point-min))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)))))

(defun ergoemacs-mac-osx-dicts (&optional layouts)
  "Generate Mac OS X dictionaries for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (mapc
     (lambda(x)
       (message "Generate Mac Dictionary for %s" x)
       (ergoemacs-gen-mac-osx x)
       (ergoemacs-gen-mac-osx x nil "os_x_opt-ctl" t))
     lay)))

(defun ergoemacs-trans-bash (key)
  "Translate Emacs kbd code KEY to bash kbd code"
  (let ((ret key)
        (case-fold-search nil))
    (with-temp-buffer
      (insert ret)
      (goto-char (point-min))
      (while (re-search-forward "\\([MSC]-\\)" nil t)
        (replace-match "\\\\\\1"))
      (setq ret (buffer-string)))
    (symbol-value 'ret)))

(defun ergoemacs-gen-bash (layout &optional file-name extra)
  "Generates an Autohotkey Script for Ergoemacs Keybindings.
Currently only supports two modifier plus key."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "bash-us.txt"))
        (xtra (or extra "bash"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (i 0))
    
    ;; ergoemacs-variable-layout
    (if (not lay)
        (message "Layout %s not found" layout)
      (ergoemacs-setup-keys-for-layout layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      
      ;; Translate keys
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout ".txt") extra-dir))
      (with-temp-file file
        (set-buffer-file-coding-system 'utf-8)
        (insert-file-contents (expand-file-name fn dir))
        (goto-char (point-min))
        (when (re-search-forward "QWERTY")
          (replace-match layout))
        (mapc
         (lambda(x)
           (let ((from (nth 0 x))
                 from-reg
                 (to nil))
             (setq to (ergoemacs-kbd from t))
             (if (string= from to) nil
               
               (setq from (ergoemacs-trans-bash from))
               (setq to (ergoemacs-trans-bash to))
               (setq from-reg (regexp-quote from))
               (goto-char (point-min))
               (when (re-search-forward from-reg nil t)
                 (replace-match to t t)))))
         (symbol-value (ergoemacs-get-variable-layout)))
        (goto-char (point-min))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)))))

(defun ergoemacs-bashs (&optional layouts)
  "Generate BASH scripts for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (mapc
     (lambda(x)
       (message "Generate bash for %s" x)
       (ergoemacs-gen-bash x))
     lay)))

(defun ergoemacs-trans-ahk (key &optional number)
  "Translates Emacs kbd code KEY to ahk kbd code. "
  (let ((ret key)
        (mod-code 0)
        (case-fold-search nil))
    
    (while (string-match "-\\([A-Z]\\)\\($\\| \\)" ret)
      (setq ret (replace-match
                 (concat "-S-"
                         (downcase (match-string 1 ret)) (match-string 2 ret))
                 t t ret)))
    
    (while (string-match "M-" ret)
      (setq mod-code (+ mod-code 2))
      (setq ret (replace-match (if number "" "!") t t ret)))
    
    (while (string-match "S-" ret)
      (setq mod-code (+ mod-code 8))
      (setq ret (replace-match (if number "" "+") t t ret)))
    
    (while (string-match "C-" ret)
      (setq mod-code (+ mod-code 4))
      (setq ret (replace-match (if number "" "^") t t ret)))
    (if (and number (= 1 (length ret)))
        (setq ret (format "%s%s" (string-to-char ret) (number-to-string mod-code))))
    (symbol-value 'ret)))

(defun ergoemacs-get-layouts-ahk-ini ()
  "Gets the easymenu entry for ergoemacs-layouts."
  (let ((lay-ini "")
        (trans-keys '())
        (i 0))
    (with-temp-buffer
      (insert "[Layouts]\n")
      (mapc
       (lambda(lay)
         (let* ((variable (intern (concat "ergoemacs-layout-" lay)))
                (alias (condition-case nil
                           (indirect-variable variable)
                         (error variable)))
                (is-alias nil)
                (doc nil))
           (setq doc (or (documentation-property variable 'variable-documentation)
                         (progn
                           (setq is-alias t)
                           (documentation-property alias 'variable-documentation))))
           (insert lay)
           (insert "=")
           (insert doc)
           (insert "\n")
           (setq i 1)
           (setq lay-ini (format "%s\n[%s]" lay-ini lay))
           (mapc
            (lambda(x)
              (unless (string-match "\\(<apps>\\|<menu>\\)" x) ;; Currently take out
                ;; <apps> mapping.  Needs some work.
                (let ((key (format "%s" (string-to-char x))))
                  (add-to-list 'trans-keys `(,x ,key))
                  (setq lay-ini (format "%s\n%s=%s" lay-ini i key))))
              (setq i (+ i 1)))
            (symbol-value variable))))
       (ergoemacs-get-layouts))
      (goto-char (point-max))
      (insert lay-ini)
      (insert "\n")
      (buffer-string))))

(defun ergoemacs-get-themes-ahk-ini ()
  "Gets the list of all known themes and the documentation associated with the themes."
  (with-temp-buffer
    (insert "[Themes]\n")
    (insert "Standard=Standard Theme\n")
    (let ((lays (sort (ergoemacs-get-themes) 'string<)))
      (mapc
       (lambda(lay)
         (let* ((variable (intern (concat "ergoemacs-" lay "-theme")))
                (alias (condition-case nil
                           (indirect-variable variable)
                         (error variable)))
                (is-alias nil)
                (doc nil))
           (setq doc (or (documentation-property variable 'group-documentation)
                         (progn
                           (setq is-alias t)
                           (documentation-property alias 'group-documentation))))
           (insert lay)
           (insert "=")
           (insert doc)
           (insert "\n")))
       lays))
    (buffer-string)))

(defun ergoemacs-get-ahk-keys-ini ()
  "Get ahk keys for all themes/ahk combinations and put into INI file."
  (flet ((ergoemacs-unset-redundant-global-keys ()))
    (let ((re "")
          lst)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir))
        (goto-char (point-min))
        (while (re-search-forward "^\\([^ \n]*\\):" nil t)
          (add-to-list 'lst (match-string 1))))
      (setq re (format "^%s$" (regexp-opt lst 't)))
      (with-temp-buffer
        (let ((old-lay ergoemacs-theme))
          (ergoemacs-set-default 'ergoemacs-theme nil)
          (mapc
           (lambda(x)
             (ergoemacs-setup-keys-for-layout x)
             (insert (concat "[" x "-Standard]\n"))
             (mapc
              (lambda(y)
                (message "Generating AHK ini for %s Standard" x)
                (when (string-match re (format "%s"(nth 1 y)))
                  (unless (string-match "\\(<apps>\\|<menu>\\)"
                                        (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y)) t))
                    (insert (symbol-name (nth 1 y)))
                    (insert "=")
                    (insert (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y)) t))
                    (insert "\n"))))
              (symbol-value (ergoemacs-get-variable-layout))))
           (ergoemacs-get-layouts))
          (mapc
           (lambda(z)
             (ergoemacs-set-default 'ergoemacs-theme z)
             (mapc
              (lambda(x)
                (ergoemacs-setup-keys-for-layout x)
                (insert (concat "[" x "-" z "]\n"))
                (message "Generating AHK ini for %s %s" x z)
                (mapc
                 (lambda(y)
                   (when (string-match re (format "%s" (nth 1 y)))
                     (unless (string-match "\\(<apps>\\|<menu>\\)" (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y))))
                       (insert (symbol-name (nth 1 y)))
                       (insert "=")
                       (insert (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y))))
                       (insert "\n"))))
                 (symbol-value (ergoemacs-get-variable-layout))))
              (ergoemacs-get-layouts)))
           (ergoemacs-get-themes))
          (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)
          (ergoemacs-set-default 'ergoemacs-theme old-lay))
        (buffer-string)))))

(defun ergoemacs-gen-ahk (&optional extra)
  "Generates autohotkey for all layouts and themes"
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode %s/ergoemacs-extras --eval \"(ergoemacs-gen-ahk)\" &"
                               (ergoemacs-emacs-exe)
                               ergoemacs-dir ergoemacs-dir)))
    (let ((xtra (or extra "ahk")) 
          not-first
          (extra-dir)
          file-temp)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq file-temp (expand-file-name "ergoemacs.ini" extra-dir))
      (with-temp-file file-temp
        (set-buffer-file-coding-system 'utf-8)
        (insert (ergoemacs-get-layouts-ahk-ini))
        (insert (ergoemacs-get-themes-ahk-ini))
        (insert (ergoemacs-get-ahk-keys-ini)))
      (setq file-temp (expand-file-name "ergoemacs.ahk" extra-dir))
      (with-temp-file file-temp
        (set-buffer-file-coding-system 'utf-8)
        (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir)))
      (message "Generated ergoemacs.ahk")
      (when (executable-find "ahk2exe")
        (shell-command (format "ahk2exe /in %s" file-temp))
        (message "Generated ergoemacs.exe")))))


;;;###autoload
(defun ergoemacs-extras ( &optional layouts)
  "Generate layout diagram, and other scripts for system-wide ErgoEmacs keybinding.

The following are generated:
• SVG Diagram for ErgoEmacs command layouts in SVG format.
• Bash 〔.inputrc〕 code.
• Mac OS X 〔DefaultKeyBinding.dict〕 code.
• AutoHotkey script for Microsoft Windows.

Files are generated in the dir 〔ergoemacs-extras〕 at `user-emacs-directory'."
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode %s/ergoemacs-extras --eval \"(ergoemacs-extras)\" &"
                               (ergoemacs-emacs-exe)
                               ergoemacs-dir ergoemacs-dir)))
    (unless ergoemacs-mode
      (ergoemacs-mode 1))
    (ergoemacs-svgs layouts)
    (ergoemacs-gen-ahk)
    (ergoemacs-bashs layouts)
    (ergoemacs-mac-osx-dicts layouts)
    (ergoemacs-get-html)
    ;; (find-file (expand-file-name "ergoemacs-extras" user-emacs-directory))
    ))

(defun ergoemacs-keyfreq-image ()
  "Create heatmap keyfreq images, based on the current layout."
  (interactive)
  (if (not (featurep 'keyfreq))
      (error "This requires the package `keyfreq'")
    (let ((table (copy-hash-table keyfreq-table))
          list
          (total-n 0)
          (cmd-n 0)
          (i 0)
          i2
          cmd-freq-ergo 
          tmp
          (lay (or (intern-soft (format "ergoemacs-layout-%s"
                                        ergoemacs-keyboard-layout))
                   'ergoemacs-layout-us))
          var-layout)
      (setq lay (symbol-value lay))
      (flet ((gen-img (file prefix text shift)
                      (let (ret i)
                        (with-temp-file file
                          (insert-file-contents (expand-file-name fn ergoemacs-dir))
                          ;; Change all text to black
                          (goto-char (point-min))
                          (while (re-search-forward "<text" nil t)
                            (when (re-search-forward "fill:#.*?;" nil t)
                              (replace-match "fill:#000000;")))

                          ;; Change the letters to A, B, C, D, and E
                          (goto-char (point-min))
                          (while (re-search-forward ">\\([MCA]\\)\\([0-9]+\\)<" nil t)
                            (cond
                             ((string= "A" (match-string 1))
                              (replace-match ">E\\2<" t))
                             ((and (string= "C" (match-string 1))
                                   (<= 60 (string-to-number (match-string 2))))
                              (replace-match (format ">C%s<" (- (string-to-number (match-string 2)) 60))))
                             ((and (string= "M" (match-string 1))
                                   (<= 60 (string-to-number (match-string 2))))
                              (replace-match (format ">A%s<" (- (string-to-number (match-string 2)) 60))))
                             ((string= "M" (match-string 1))
                              (replace-match ">B\\2<"))
                             ((string= "C" (match-string 1))
                              (replace-match ">D\\2<"))))
                          
                          ;; Now add the layout information.
                          (setq i 0)
                          ;; (setq cmd-freq '())
                          (while (< i (length lay))
                            (goto-char (point-min))
                            (when (search-forward (format ">%s<" i) nil t)
                              (if (string= "" (nth (+ (if shift 60 0) i) lay))
                                  (replace-match "><")
                                (replace-match (format ">%s<"
                                                       (ergoemacs-gen-svg-quote
                                                        (upcase (nth
                                                                 (+ (if shift 60 0) i)
                                                                 lay)))) t t)))
                            (setq tmp (assoc (format "%s%s" prefix
                                                     (nth (+ (if shift 60 0) i) lay))
                                             cmd-freq-ergo))
                            (if (not tmp)
                                (progn
                                  ;; Try to figure out if this is a
                                  ;; prefix, or not...
                                  (setq tmp (all-completions
                                             (format "%s%s " prefix (nth (+ (if shift 60 0) i) lay))
                                             cmd-freq-ergo))
                                  (if tmp
                                      (progn
                                        (setq ret t)
                                        (goto-char (point-min))
                                        (while (re-search-forward (format ">A%s<" i) nil t)
                                          (replace-match ">♦<"))
                                        
                                        (goto-char (point-min))
                                        (while (re-search-forward (format ">[BCDE]%s<" i) nil t)
                                          (replace-match "><"))
                                        
                                        (goto-char (point-min))
                                        (when (re-search-forward (format "id=\"key%s\"" i) nil t)
                                          (when (re-search-backward "fill:.*?;" nil t)
                                            (replace-match "fill:#00FFFF;")))
                                        (let ((new-file
                                               (replace-regexp-in-string
                                                ".svg"
                                                (format "-%s%s.svg"
                                                        (if shift "S-" "")
                                                        (nth (+ (if shift 60 0) i) lay))
                                                file)))
                                          
                                          (unless (gen-img new-file
                                                           (format "%s%s "
                                                                   prefix
                                                                   (nth (+ (if shift 60 0) i) lay))
                                                           (format "%s%s" text
                                                                   (nth (+ (if shift 60 0) i) lay)) nil)
                                            (delete-file new-file))

                                          (setq new-file
                                                (replace-regexp-in-string
                                                 ".svg" "-S.svg"
                                                 new-file))
                                          (unless (gen-img new-file
                                                           (format "%s%s "
                                                                   prefix
                                                                   (nth (+ (if shift 60 0) i) lay))
                                                           (format "%s%s ⇧Shift+" text
                                                                   (nth (+ (if shift 60 0) i) lay)) t)
                                            (delete-file new-file))))
                                    (goto-char (point-min))
                                    (while (re-search-forward (format ">[ABCDE]%s<" i) nil t)
                                      (replace-match "><"))
                                    (goto-char (point-min))
                                    (when (re-search-forward (format "id=\"key%s\"" i) nil t)
                                      (when (re-search-backward "fill:.*?;" nil t)
                                        (replace-match "fill:#FFFF00;")))))
                              (setq ret t)
                              (goto-char (point-min))
                              (when (search-forward (format "id=\"key%s\"" i) nil t)
                                (when (re-search-backward "fill:.*?;" nil t)
                                  (replace-match (format "fill:%s;" (nth 6 tmp)))))
                              ;;(add-to-list 'cmd-freq (cons (nth 2 tmp) (format "id=\"key%s\"" i)))
                              (goto-char (point-min))
                              (when (search-forward (format ">A%s<" i) nil t)
                                (replace-match (format ">%s<" (nth 1 tmp)) t t))
                              (goto-char (point-min))
                              (when (search-forward (format ">B%s<" i) nil t)
                                (replace-match (format ">N: %s<" (nth 2 tmp)) t t))
                              (goto-char (point-min))
                              (when (search-forward (format ">C%s<" i) nil t)
                                (replace-match (format ">NC: %s<" (length (nth 3 tmp))) t t))
                              (goto-char (point-min))
                              (when (search-forward (format ">D%s<" i) nil t)
                                (replace-match (format ">Cmd: %s<" (nth 4 tmp)) t t))
                              (goto-char (point-min))
                              (when (search-forward (format ">E%s<" i) nil t)
                                (replace-match (format ">Tot: %s<" (nth 5 tmp)) t t)))
                            (setq i (+ i 1)))
                          ;; Now lookup prefix-SPC
                          (setq tmp (assoc (format "%s%sSPC" prefix (if shift "S-" ""))
                                           cmd-freq-ergo))
                          (if (not tmp)
                              (progn
                                (goto-char (point-min))
                                (while (re-search-forward ">\\(MS\\|MM\\|CS\\|CC\\|AA\\)-SPC<" nil t)
                                  (replace-match "><"))
                                (goto-char (point-min))
                                (when (re-search-forward "id=\"keySPC\"" nil t)
                                  (when (re-search-backward "fill:.*?;" nil t)
                                    (replace-match "fill:#FFFF00;"))))
                            ;;(add-to-list 'cmd-freq (cons (nth 2 tmp)
                            ;;"id=\"keySPC\"")
                            (setq ret t)
                            (goto-char (point-min))
                            (when (search-forward ">MS-SPC<" nil t)
                              (replace-match (format ">%s<" (nth 1 tmp)) t t))
                            (goto-char (point-min))
                            (when (search-forward ">MM-SPC<" nil t)
                              (replace-match (format ">N: %s<" (nth 2 tmp)) t t))
                            (goto-char (point-min))
                            (when (search-forward ">CS-SPC<" nil t)
                              (replace-match (format ">NC: %s<" (length (nth 3 tmp))) t t))
                            (goto-char (point-min))
                            (when (search-forward ">CC-SPC<" nil t)
                              (replace-match (format ">Cmd: %s<" (nth 4 tmp)) t t))
                            (goto-char (point-min))
                            (when (search-forward ">AA-SPC<" nil t)
                              (replace-match (format ">Tot: %s<" (nth 5 tmp)) t t)))
                          (goto-char (point-min))
                          (when (search-forward ">title<" nil t)
                            (replace-match (format ">Frequency Heatmap for %s<" text)))
                          (goto-char (point-min))
                          (when (search-forward ">MS<" nil t)
                            (replace-match ">N: Number of times called<" t))
                          (goto-char (point-min))
                          (when (search-forward ">MM<" nil t)
                            (replace-match ">NC: Number of commands for this key<" t))
                          (goto-char (point-min))
                          (when (search-forward ">CS<" nil t)
                            (replace-match ">Cmd: % of emacs keyboard commands<" t))
                          (goto-char (point-min))
                          (when (search-forward ">CC<" nil t)
                            (replace-match ">Tot: % of typing<" t))
                          (goto-char (point-min))
                          (while (re-search-forward ">\\(AA\\)<" nil t)
                            (replace-match "><" t)))
                        (symbol-value 'ret)))
             (calc-ergo (x)
                        (let ((a (assoc (nth 1 x) (cdr list)))
                              curr-cmd
                              (cmds '())
                              (num 0))
                          (when a
                            (setq num (+ num (cdr a)))
                            (add-to-list 'cmds (car a)))
                          ;; Now lookup key based on the currently installed
                          ;; minor modes

                          ;; For example with subword-mode, backward-word
                          ;; becomes subword-backward-word
                          (setq curr-cmd
                                (key-binding (if var-layout
                                                 (ergoemacs-kbd (nth 0 x) nil (nth 3 x))
                                               (read-kbd-macro (nth 0 x))) t))
                          
                          (unless (memq curr-cmd cmds)
                            (setq a (assoc curr-cmd (cdr list)))
                            (when a
                              (setq num (+ num (cdr a)))
                              (add-to-list 'cmds (car a))))
                          ;; Also lookup based on any compatibility fixes with
                          ;; made by ergoemacs. 
                          (mapc
                           (lambda(minor-list)
                             (mapc
                              (lambda(translation-list)
                                (when (eq (nth 1 x) (nth 0 translation-list))
                                  (setq a (assoc (nth 1 translation-list) (cdr list)))
                                  (when a
                                    (setq num (+ num (cdr a)))
                                    (add-to-list 'cmds (car a)))))
                              (nth 1 minor-list)))
                           (symbol-value (ergoemacs-get-minor-mode-layout)))
                          (list (if var-layout
                                    (ergoemacs-kbd (nth 0 x) t (nth 3 x))
                                  (nth 0 x)) (nth 2 x)  num cmds
                                  (format "%6.2f%%" (/ (* 1e2 num) cmd-n))
                                  (format "%6.2f%%" (/ (* 1e2 num) total-n))))))
        
        ;; Merge with the values in .emacs.keyfreq file
        (keyfreq-table-load table)
        (setq list (keyfreq-list (keyfreq-groups-major-modes table) 'no-sort))
        (mapc
         (lambda(x)
           (setq total-n (+ total-n (cdr x)))
           (unless (string-match "self-insert" (symbol-name (car x)))
             (setq cmd-n (+ cmd-n (cdr x)))))
         (cdr list))

        ;; Get the frequencies for all the ergoemacs commands
        (setq cmd-freq-ergo
              (mapcar
               'calc-ergo 
               (append
                (symbol-value (ergoemacs-get-fixed-layout)))))
        (setq var-layout t)
        
        (setq cmd-freq-ergo
              (append cmd-freq-ergo
                      (mapcar
                       'calc-ergo 
                       (append
                        (symbol-value (ergoemacs-get-variable-layout))))))
        
        (setq cmd-freq-ergo (sort cmd-freq-ergo #'(lambda(x y) (< (nth 2 x) (nth 2 y)))))

        ;; Consolidated color calculation
        (setq i2 (/ (* 1e0 (length cmd-freq-ergo)) 2.0))
        (setq i 0)
        
        (setq cmd-freq-ergo
              (mapcar
               (lambda(x)
                 (let (tmp color)
                   (cond
                    ((< i i2)
                     (setq tmp (* 255e0 (/ (* 1e0 i) (* 1e0 i2))))
                     (setq color (format "#%02X%02Xff" tmp tmp)))
                    (t
                     (setq tmp (* 255e0 (- 1e0 (/ (- (* 1e0 i) (* 1e0 i2)) (* 1e0 i2)) )))
                     (setq color (format "#ff%02X%02X" tmp tmp))))
                   (setq i (+ i 1))
                   (append x (list color))))
               cmd-freq-ergo))
        (let ((fn "kbd-ergo.svg")
              extra-dir)
          (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
          (if (not (file-exists-p extra-dir))
              (make-directory extra-dir t))
          (setq file (expand-file-name  "keyfreq-alt-map.svg" extra-dir))
          (gen-img file "M-" "Alt+" nil)
          (message "Generated Alt+ frequency heatmap")
          
          (setq file (expand-file-name  "keyfreq-alt-shift-map.svg" extra-dir))
          (gen-img file "M-" "Alt+⇧Shift+" t)
          (message "Generated Alt+⇧Shift+ frequency heatmap")
          
          (setq file (expand-file-name  "keyfreq-ctrl-map.svg" extra-dir))
          (gen-img file "C-" "Ctrl+" nil)
          (message "Generated Ctrl+ frequency heatmap")
          
          (setq file (expand-file-name  "keyfreq-ctrl-shift-map.svg" extra-dir))
          (gen-img file "C-" "Ctrl+⇧Shift+" t)
          (message "Generated Ctrl+⇧Shift+ frequency heatmap")

          (setq file (expand-file-name  "keyfreq-menu-map.svg" extra-dir))
          (gen-img file (if (eq system-type 'windows-nt)
                            "<apps> "
                          "<menu> ") "▤ Menu/Apps" nil)
          (message "Generated ▤ Menu/Apps")
          )))))

;; Allow the SVN prefixes to be specified by the following:
(setq ergoemacs-svn-prefixes
  '(("M-" "M" "MS" "Alt+⇧Shift+" "M-S-" nil)
    ("M-" "M" "MM" "Alt+" "M-" nil)
    ("C-" "C" "CS" "Ctrl+⇧Shift+" "C-S-" nil)
    ("C-" "C" "CC" "Ctrl+" "C-" nil)
    ("<apps> " "A" "AA" "▤ Menu/Apps " "<apps> " nil)))

;;; Format of list -- (0) Emacs prefix (1) svg prefix (2) Final/Symbol
;;; prefix (3) Final text legend (4) Keyboard lookup (5) Treat
;;; variable-layout as fixed-layout

(defun ergoemacs-gen-svg (layout &optional file-name extra is-prefix)
  "Generates a SVG picture of the layout
FILE-NAME represents the SVG template
EXTRA represents an extra file representation.
IS-PREFIX tell ergoemacs if this is a prefix diagram."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "kbd.svg"))
        (xtra (or extra "kbd-layouts"))
        file
        txt
        (prefix-lst '())
        (prefix-num 0)
        prefix-fixed
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (fix (mapcar
              (lambda(x)
                `(,(if (condition-case err
                           (string-match "-S-\\([a-z]\\)\\>" (nth 0 x))
                         (error nil))
                       (replace-match (format "-%s" (upcase (match-string 1 (nth 0 x)))) t t (nth 0 x))
                     (nth 0 x))  ,(nth 1 x) ,(nth 2 x)))
              `(,@(symbol-value (ergoemacs-get-fixed-layout))
                ,@(if cua-mode
                      `(("C-c" nil "Copy")
                        ("C-v" nil "Paste")
                        ("C-x" nil "Cut"))
                    nil))))
        (i 0))
    (if (not lay)
        (message "Layout %s not found" layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq lay (symbol-value lay))
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout (if is-prefix "-prefix" "") ".svg") extra-dir))
      (with-temp-file file
        ;;(set-buffer-file-coding-system 'utf-8)
        (insert-file-contents
         (expand-file-name fn dir))
        (when is-prefix
          (goto-char (point-min))
          (while (search-forward ">A" nil t)
            (replace-match ">4A"))
          (goto-char (point-min))
          (while (re-search-forward ">M\\([0-9]+\\)<" nil t)
            (if (<= 60 (string-to-number (match-string 1)))
                (replace-match (format ">0A%s<" (- (string-to-number (match-string 1)) 60)) t t)
              (replace-match (format ">1A%s<" (match-string 1)) t t)))
          (goto-char (point-min))
          (while (re-search-forward ">C\\([0-9]+\\)<" nil t)
            (if (<= 60 (string-to-number (match-string 1)))
                (replace-match (format ">2A%s<" (- (string-to-number (match-string 1)) 60)) t t)
              (replace-match (format ">3A%s<" (match-string 1)) t t)))
          (goto-char (point-min))
          (while (re-search-forward ">MS" nil t)
            (replace-match ">0AA" t t))
          (goto-char (point-min))
          (while (re-search-forward ">MM" nil t)
            (replace-match ">1AA" t t))
          (goto-char (point-min))
          (while (re-search-forward ">CS" nil t)
            (replace-match ">2AA"))
          (goto-char (point-min))
          (while (re-search-forward ">CC" nil t)
            (replace-match ">3AA")))
        (when (string-equal system-type "windows-nt")
          ;; Use Arial Unicode MS when on windows
          (goto-char (point-min))
          (while (re-search-forward "\\(?:Helvetica\\|Sans\\)\\([\";]\\)" nil t)
            (replace-match "Arial Unicode MS\\1")))
        (while (< i (length lay))
          (goto-char (point-min))
          (when (search-forward (format ">%s<" i) nil t)
            (replace-match (format ">%s<" (ergoemacs-gen-svg-quote (nth i lay))) t t))
          (mapc
           (lambda(x)
             (let ((key-pre (nth 0 x))
                   (rep-pre (nth 1 x))
                   (rep-sym-pre (nth 2 x))
                   (final-txt (nth 3 x))
                   (sym-pre (nth 4 x))
                   (var-is-fixed-p (nth 5 x))
                   curr-var)
               ;; Variable Layout
               (setq curr-var (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from)))))
               
               (if var-is-fixed-p
                   (setq curr-var (nth i lay))
                 (setq curr-var (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))))
               
               (unless (string= curr-var "")
                 (setq txt (assoc (format "%s%s" key-pre curr-var)
                                  (symbol-value (ergoemacs-get-variable-layout))))
                 
                 (if (not txt)
                     (setq txt "")
                   (if (>= (length txt) 3)
                       (setq txt (nth 2 txt))
                     (setq txt "")))
                 (when (string= "" txt)
                   (setq txt (all-completions (format "%s%s " key-pre curr-var)
                                              (symbol-value (ergoemacs-get-variable-layout))))
                   (if (= 0 (length txt))
                       (setq txt "")
                     (setq prefix-fixed (nth 3 (assoc (nth 0 txt) (symbol-value (ergoemacs-get-variable-layout)))))
                     (setq txt "♦")
                     ;; Add to prefix list
                     (setq prefix-lst
                           (append prefix-lst
                                   (list
                                    (list
                                     (format "%s%s " key-pre curr-var)
                                     (format "%sA" prefix-num)
                                     (format "%sAA" prefix-num)
                                     (format "%s%s" final-txt (nth i lay))
                                     (format "%sAA-" prefix-num)
                                     prefix-fixed))))
                     (setq prefix-num (+ prefix-num 1))))
                 (goto-char (point-min))
                 (unless (string= "" txt)
                   (when (search-forward (format ">%s%s<" rep-pre i) nil t)
                     (replace-match  (format ">%s<" txt) t t))))
               
               ;; Fixed layout
               (goto-char (point-min))
               (setq txt (assoc (format "%s%s" key-pre (nth i lay)) fix))
               (if (not txt)
                   (setq txt "")
                 (if (>= (length txt) 3)
                     (setq txt (nth 2 txt))
                   (setq txt "")))
               (when (string= txt "")
                 (setq txt (all-completions (format "%s%s " key-pre (nth i lay)) fix))
                 (if (= 0 (length txt))
                     (setq txt "")
                   (setq txt "♦")))
               (unless (string= "" txt)
                 (when (search-forward (format ">%s%s<" rep-pre i) nil t)
                   (replace-match  (format ">%s<" txt) t t)))
               ;; Space and other symbols
               (mapc
                (lambda(sym)
                  (setq txt (assoc (format "%s%s" sym-pre sym) (symbol-value (ergoemacs-get-variable-layout))))
                  (if (not txt)
                      (setq txt "")
                    (if (>= (length txt) 3)
                        (setq txt (nth 2 txt))
                      (setq txt "")))
                  
                  (when (string= "" txt)
                    (setq txt
                          (all-completions
                           (format "%s%s " sym-pre sym)
                           (symbol-value (ergoemacs-get-variable-layout))))
                    (if (= 0 (length txt))
                        (setq txt "")
                      (setq txt "♦")))
                  (goto-char (point-min))
                  (unless (string= "" txt)
                    (when (search-forward (format ">%s-%s<" rep-sym-pre sym) nil t)
                      (replace-match  (format ">%s<" (ergoemacs-gen-svg-quote txt)) t t))))
                '("SPC"))
               
               ;; Legend/Key
               (goto-char (point-min))
               (when (search-forward (format ">%s<" rep-sym-pre) nil t)
                 (replace-match (format ">%s<" (ergoemacs-gen-svg-quote final-txt)) t t))))
           ergoemacs-svn-prefixes)
          (setq i (+ i 1)))
        (goto-char (point-min))
        (while (re-search-forward ">\\([0-4]?[CMA][0-9]+\\|[0-4]?[CMA]\\{2\\}.*?\\|nil\\)<" nil t)
          (replace-match "><"))
        (goto-char (point-min))
        (when (search-forward ">title<" nil t)
          (if is-prefix
              (replace-match ">Continuation of Emacs Command Sequences:<")
            (replace-match (format
                            ">Layout: %s; Theme %s<"
                            layout
                            (or ergoemacs-theme "Standard"))))))
      (when ergoemacs-inkscape
        (message "Converting to png")
        (shell-command (format "%s -z -f \"%s\" -e \"%s\"" ergoemacs-inkscape
                               file (concat (file-name-sans-extension file) ".png"))))
      (message "Layout generated to %s" file)
      (when prefix-lst
        (let ((ergoemacs-svn-prefixes prefix-lst))
          (ergoemacs-gen-svg layout file-name extra t)))
      (when (and is-prefix ergoemacs-convert ergoemacs-inkscape)
        (message "Concatenating layouts.")
        ;; Concatenate two files
        (shell-command
         (format "%s -append \"%s\" \"%s\" \"%s\"" ergoemacs-convert
                 (replace-regexp-in-string "-prefix.svg" ".png" file)
                 (replace-regexp-in-string ".svg" ".png" file)
                 (replace-regexp-in-string ".svg" "-tmp.png" file)))
        (copy-file
         (replace-regexp-in-string ".svg" "-tmp.png" file)
         (replace-regexp-in-string "-prefix.svg" ".png" file) t)
        (delete-file (replace-regexp-in-string ".svg" "-tmp.png" file))))))


(defun ergoemacs-curr-svg ()
  "Generates the current ergoemacs layout, unless it already exists."
  (interactive)
  (let ((var ergoemacs-theme)
        (layout ergoemacs-keyboard-layout)
        (extra "ergo-layouts")
        (dir "")
        (file ""))
    (when var
      (setq extra (concat var "/ergo-layouts")))
    (setq dir (expand-file-name extra
                                (expand-file-name "ergoemacs-extras" user-emacs-directory)))
    (setq file (expand-file-name (concat "ergoemacs-layout-" layout ".svg") dir))
    (unless (file-exists-p file)
      (message "Generating SVG file...")
      (unless (featurep 'ergoemacs-extras)
        (require 'ergoemacs-extras))
      (ergoemacs-gen-svg layout "kbd-ergo.svg" extra)
      (message "Generated!"))
    (when (called-interactively-p 'interactive)
      (browse-url (concat "file://" file)))
    (symbol-value 'file)))

;;;###autoload
(defun ergoemacs-svgs (&optional layouts)
  "Generate SVGs for all the defined layouts and themes."
  (interactive)
  (let* ((lay (or layouts (ergoemacs-get-layouts)))
         (saved-theme ergoemacs-theme))
    (mapc
     (lambda(x)
       (message "Generate SVG for %s" x)
       (condition-case err
           (progn
             (ergoemacs-gen-svg x)
             (ergoemacs-set-default 'ergoemacs-theme nil)
             (ergoemacs-gen-svg x "kbd-ergo.svg" "ergo-layouts")) 
         (error (message "Error generating base SVG for %s; %s" x err)))
       (mapc
        (lambda(y)
          (condition-case err
              (progn
                (ergoemacs-set-default 'ergoemacs-theme y)
                (ergoemacs-gen-svg x "kbd-ergo.svg" (concat y)))
            (error (message "Error generating theme %s; %s" y err))))
        (sort (ergoemacs-get-themes) 'string<))
       (message "Setting theme back to %s" saved-theme)
       (ergoemacs-set-default 'ergoemacs-theme saved-theme))
     lay)))

(provide 'ergoemacs-extras)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-extras.el ends here
