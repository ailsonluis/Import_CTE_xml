*&---------------------------------------------------------------------*
*& Report ZABAP_IMPORT_XML_CTE
*&---------------------------------------------------------------------*
*& Escrituração de CTE na apartir do XML
*&
*&---------------------------------------------------------------------*
report zt41_26_import_xml_cte.

"Classe de erro
class lcx_msg definition
    inheriting from cx_static_check.

endclass.

"classe do programa
class lcl_app definition.
  public section.


" estrutura para os arquivos do diretorio
types:
      begin of ty_file,
        directory(75) type c,
        name(75)      type c,
        type(10)      type c,
        len(8)        type p decimals 0,
        owner(8)      type c,
        mtime(6)      type p decimals 0,
        mode(9)       type c,
        errno(3)      type c,
        errmsg(40)    type c,
        mod_date      type d,
        mod_time(8)   type c,
        subrc         type sy-subrc,
      end of ty_file,

" Estrutura para gravar os dados do CTE ( Cabeçalho e Item)
      begin of ty_alv,
        icon            type icon_d,
        docnum          type j_1bdocnum,
        access_key      type j_1b_nfe_access_key_dtel44,
        xmlvers         type bapi_cte_j_1bnfdoc-xmlvers,
        model           type bapi_cte_j_1bnfdoc-model,
        nfenum          type bapi_cte_j_1bnfdoc-nfenum,
        series          type bapi_cte_j_1bnfdoc-series,
        docdat          type bapi_cte_j_1bnfdoc-docdat,
        authcod         type bapi_cte_j_1bnfdoc-authcod,
        emit_cnpj       type j_1bstcd1,
        emit_xnome      type name1_gp,
        parid           type bapi_cte_j_1bnfdoc-parid,
        rem_cnpj        type j_1bcgc,
        rem_xnome       type name1,
        bukrs           type bapi_cte_j_1bnfdoc-bukrs,
        branch          type bapi_cte_j_1bnfdoc-branch,
        werks           type bapi_cte_j_1bnflin-werks,
        cfpo_10         type bapi_cte_j_1bnflin-cfop,
        netpr           type bapi_cte_j_1bnflin-netpr,
        cte_strt_lct    type bapi_cte_j_1bnfdoc-cte_strt_lct,
        cte_end_lct     type bapi_cte_j_1bnfdoc-cte_end_lct,
        rate            type bapi_cte_j_1bnfstx-rate,
        taxval          type bapi_cte_j_1bnfstx-taxval,
        base            type bapi_cte_j_1bnfstx-base,
      end of ty_alv,

      begin of ty_cte,
        icon            type icon_d,
        docnum          type j_1bdocnum,
        emit_cnpj       type j_1bstcd1,
        emit_xnome      type name1_gp,
        rem_cnpj        type j_1bcgc,
        rem_xnome       type name1,
        access_key      type bapi_cte_j_1bnfdoc-access_key,
        nftype          type bapi_cte_j_1bnfdoc-nftype,
        doctyp          type bapi_cte_j_1bnfdoc-doctyp,
        direct          type bapi_cte_j_1bnfdoc-direct,
        docdat          type bapi_cte_j_1bnfdoc-docdat,
        pstdat          type bapi_cte_j_1bnfdoc-pstdat,
        model           type bapi_cte_j_1bnfdoc-model,
        series          type bapi_cte_j_1bnfdoc-series,
        manual          type bapi_cte_j_1bnfdoc-manual,
        waerk           type bapi_cte_j_1bnfdoc-waerk,
        bukrs           type bapi_cte_j_1bnfdoc-bukrs,
        branch          type bapi_cte_j_1bnfdoc-branch,
        parvw           type bapi_cte_j_1bnfdoc-parvw,
        parid           type bapi_cte_j_1bnfdoc-parid,
        partyp          type bapi_cte_j_1bnfdoc-partyp,
        nfe             type bapi_cte_j_1bnfdoc-nfe,
        nfenum          type bapi_cte_j_1bnfdoc-nfenum,
        authcod         type bapi_cte_j_1bnfdoc-authcod,
        docstat         type bapi_cte_j_1bnfdoc-docstat,
        xmlvers         type bapi_cte_j_1bnfdoc-xmlvers,
        docnum9         type bapi_cte_j_1bnfdoc-docnum9,
        authdate        type bapi_cte_j_1bnfdoc-authdate,
        authtime        type bapi_cte_j_1bnfdoc-authtime,
        tpemis          type bapi_cte_j_1bnfdoc-tpemis,
        cte_strt_lct    type bapi_cte_j_1bnfdoc-cte_strt_lct,
        cte_end_lct     type bapi_cte_j_1bnfdoc-cte_end_lct,
        chekcon         type bapi_cte_j_1bnfcheck-chekcon,
        mandt           type bapi_cte_j_1bnflin-mandt,
        itmnum          type bapi_cte_j_1bnflin-itmnum,
        matnr           type bapi_cte_j_1bnflin-matnr,
        bwkey           type bapi_cte_j_1bnflin-bwkey,
        matkl           type bapi_cte_j_1bnflin-matkl,
        maktx           type bapi_cte_j_1bnflin-maktx,
        nbm             type bapi_cte_j_1bnflin-nbm,
        menge           type bapi_cte_j_1bnflin-menge,
        meins           type bapi_cte_j_1bnflin-meins,
        netpr           type bapi_cte_j_1bnflin-netpr,
        netwr           type bapi_cte_j_1bnflin-netwr,
        taxlw1          type bapi_cte_j_1bnflin-taxlw1,
        taxlw2          type bapi_cte_j_1bnflin-taxlw2,
        itmtyp          type bapi_cte_j_1bnflin-itmtyp,
        incltx          type bapi_cte_j_1bnflin-incltx,
        werks           type bapi_cte_j_1bnflin-werks,
        cfop_10         type bapi_cte_j_1bnflin-cfop_10,
        taxlw4          type bapi_cte_j_1bnflin-taxlw4,
        taxlw5          type bapi_cte_j_1bnflin-taxlw5,
        matuse          type bapi_cte_j_1bnflin-matuse,
        matorg          type bapi_cte_j_1bnflin-matorg,
        rate            type bapi_cte_j_1bnfstx-rate,
        taxval          type bapi_cte_j_1bnfstx-taxval,
        base            type bapi_cte_j_1bnfstx-base,
      end of ty_cte,

"Estrutura para os dados de escrituração de impostos do CTE
      begin of ty_cte_stx,
        chcte    type j_1b_nfe_access_key_dtel44,
        mandt type bapi_cte_j_1bnfstx-mandt,
        itmnum type bapi_cte_j_1bnfstx-itmnum,
        taxtyp type bapi_cte_j_1bnfstx-taxtyp,
        rate type bapi_cte_j_1bnfstx-rate,
        taxval type bapi_cte_j_1bnfstx-taxval,
        othbas type bapi_cte_j_1bnfstx-othbas,
        execbas type bapi_cte_j_1bnfstx-excbas,
        base type bapi_cte_j_1bnfstx-base,

      end of ty_cte_stx,


"Estrutura para os dados de Notas Fiscais atribuidas ao CTE.
      begin of ty_cte_nfe,
         chcte    type j_1b_nfe_access_key_dtel44,
         chnfe    type j_1b_nfe_access_key_dtel44,
         mandt    type mandt,
         counter  type bapi_j_1bcte_d_docref-counter,
         regio    type bapi_j_1bcte_d_docref-regio,
         nfyear   type bapi_j_1bcte_d_docref-nfyear,
         nfmonth  type bapi_j_1bcte_d_docref-nfmonth,
         stcd1    type bapi_j_1bcte_d_docref-stcd1,
         model    type bapi_j_1bcte_d_docref-model,
         series   type bapi_j_1bcte_d_docref-serie,
         nfnum9   type bapi_j_1bcte_d_docref-nfnum9,
         docnum9  type bapi_j_1bcte_d_docref-docnum9,
         cdv      type bapi_j_1bcte_d_docref-cdv,
         docval   type bapi_j_1bcte_d_docref-docval,

      end of ty_cte_nfe,


      begin of ty_string,
        string      type string,
        checkstatus type c length 3,
      end of ty_string,

       begin of ty_str_read_xml,
          tag(100) type c,
          value(200) type c,
          parent(100) type c,
          ref(50) type c,
       end of ty_str_read_xml,

"Estrutura para os dados da Filial/Empresa/Centro/CNPJ
       begin of ty_werks,
         werks type werks_d,
         j_1bbranch type j_1bbranc_,
         bukrs type bukrs,
         stcd1 type j_1bcgc,
       end of ty_werks,

*********eliminar e considerar a estrutura igual a tabela de de-para a ser utilizada
        begin of ty_depara,
          werks type werks_d,
          ycfopde(4) type c ,
          ycfoppara(7) type c,
          transportadora type lifnr,
          matnr type matnr,
          nftype type j_1bnftype,
          taxlw1 type j_1btaxlw1,
          tpicms(4) type c,
          monticms(1) type c,
          mbeicms(8) type p decimals 2,
          obicms(8) type p decimals 2,
          taxlw2 type j_1btaxlw2,
          tpipi(4) type c,
          obipi(8) type p decimals 2,
          taxlw5 type j_1btaxlw5,
          tppis(4) type c ,
          montpis(1) type c ,
          txpis(8) type p decimals 2,
          mbepis(8) type p decimals 2,
          obpis(8) type p decimals 2,
          taxlw4 type j_1btaxlw4,
          tpcofins(4) type c,
          montcofins(1) type c,
          txcofins(8) type p decimals 2,
          mbecofins(8) type p decimals 2,
          obcofins(8) type p decimals 2,
          "TAXLW6 type J_1BTAXLW6,
        end of ty_depara.
*********


class-data: v_fieldname type dynfnam.
"nome da pastas dos arquivos processados
constants gv_processados type string  value 'processados/'.

data: r_alv type ref to cl_salv_table,
      st_return   type ty_str_read_xml,
      it_return   type table of ty_str_read_xml,
      "gt_alv      type table of ty_alv,
      "wa_alv      type ty_alv,
      gt_cte   type table of ty_cte,
      wa_cte   type ty_cte,

      gt_ctenfe   type table of ty_cte_nfe,
      wa_ctenfe   type ty_cte_nfe,
      gt_ctestx   type table of ty_cte_stx,
      gt_werks    type table of ty_werks.
     " gt_depara type table of ydtrcfopdepara.

  types ty_value type c length 1.
  data : vl_value type ty_value,
         v_mode      type c.
  data : gv_path type ibipparms-path,
         r_xml_doc    type ref to cl_xml_document.

  class-methods class_constructor.

  "importa os dados do XML e processa os registros importados
  methods import_file importing i_path type ibipparms-path optional .

  "imposta os arquivos XML da pasta indicada
  methods import_folder importing i_path type ibipparms-path optional.


  "valida se o arquivo indicado existe
  methods check_file_path importing i_path  type ibipparms-path optional
                          returning value(re_result) type abap_bool
                          raising lcx_msg.

  "valida se a pasta indicada existe
  methods check_folder_path importing i_path  type ibipparms-path optional
                          returning value(re_result) type abap_bool
                          raising lcx_msg.

  "verifica se existe a pasa processados, utilizada para mover arquivos ja escriturados
  methods check_folder_process.

  "retorna os dados utilizados como depara para escrituração do CTE
  methods get_depara.

  "retornar todos centro, filial e empresa.
  methods get_all_werks .

  "Retorna os dados de centro, filial empresa apartir do cpnj
  methods get_werks importing i_cnpj type j_1bcgc
                    returning value(re_werks) type ty_werks.

  "retorna codigo do fornecedor
  methods get_lifnr importing i_cnpj type j_1bcgc
                    returning value(re_lifnr) type lifnr.

  "retorna o codigo do documento registrado na j1b1n
  methods get_docnum importing  chcte    type j_1b_nfe_access_key_dtel44
                     returning value(re_docnum) type j_1bdocnum.

  "monta a grade de escrituração fiscal dos impostos para cada cte
  methods set_ctestx importing i_cte type ty_cte.

  "registra o log para o registro em processamento
  methods set_log importing i_cte type ty_cte
                     returning value(re_icon) type icon_d.

  "cria o log para ser utilizado no processamento dos registros
  methods create_log .

  " adiciona uma ou mais mensagem de erro para o LOG,
  methods add_log importing i_msg type bal_s_msg.

  "Mostra as mensagens de erro do LOG
  methods show_log.

  "efetua a escrituração do Cte
  methods process_cte.

  "demonstra em ALV os registros importados no XML e processados
  methods view_alv.

  "controa os eventos nos botoes do ALV
  methods on_user_command for event added_function of cl_salv_events importing e_salv_function.

  "copia o arquivo que foi processado para pasta "Processado", remove o arquivo da pasta atual
  methods movefile.

  protected section.

  private section.
  "metodos visiveis somente para a instancia

endclass.

class lcl_app implementation.
  method class_constructor.


  endmethod.
  method check_file_path.
     data:
      lv_path   type string.
     " lv_result type abap_bool.

    if i_path is initial.
       lv_path = gv_path.
    else.
       lv_path = i_path.
    endif.

    call method cl_gui_frontend_services=>file_exist
      exporting
        file                 = lv_path
      receiving
        result               = re_result
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        others               = 5.
    if sy-subrc <> 0.
      raise exception type lcx_msg.
*      case sy-subrc.
*        when 1.
*          "raise cntl_error.
*        when 2.
*          "raise error_no_gui.
*        when 3.
*          "raise wrong_parameter.
*        when 4.
*          "raise not_supported_by_gui.
*      endcase.
    endif.

*    if lv_result is initial.
*      leave list-processing.
*    endif.
    if re_result is initial.
      raise exception type lcx_msg.
    endif.
  endmethod.

  method check_folder_process.
    data : lv_dir_exist type abap_bool,
           lv_path type string,
           lv_file type string.

    lv_path = gv_path.

    "verifica se existe pasta Processados, se não cria uma nova pasta
  call function 'SO_SPLIT_FILE_AND_PATH'
    exporting
      full_name           = lv_path
   importing
     stripped_name       = lv_file
     file_path           = lv_path
   exceptions
     x_error             = 1
     others              = 2
            .
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
  cl_gui_frontend_services=>directory_exist(
      exporting
        directory            = lv_path && gv_processados
      receiving
        result               = lv_dir_exist
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        others               = 5
    ).
    if sy-subrc <> 0.
*     message id sy-msgid type sy-msgty number sy-msgno
*       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    if lv_dir_exist = abap_false.
      data lv_rc type i.
      cl_gui_frontend_services=>directory_create(
        exporting
          directory                = lv_path && gv_processados
        changing
          rc                       = lv_rc
        exceptions
          directory_create_failed  = 1
          cntl_error               = 2
          error_no_gui             = 3
          directory_access_denied  = 4
          directory_already_exists = 5
          path_not_found           = 6
          unknown_error            = 7
          not_supported_by_gui     = 8
          wrong_parameter          = 9
          others                   = 10
      ).
      if sy-subrc <> 0.
*       message id sy-msgid type sy-msgty number sy-msgno
*         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

  endmethod.

  method check_folder_path.
     data:
      lv_path   type string.
     " lv_result type abap_bool.

    if i_path is initial.
      lv_path = gv_path.
    else.
      lv_path = i_path.
    endif.

    cl_gui_frontend_services=>directory_exist(
      exporting
        directory            = lv_path
      receiving
        result               = re_result
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        others               = 5 ) .
    if sy-subrc <> 0.
      raise exception type lcx_msg.
*      case sy-subrc.
*        when 1.
*          "raise cntl_error.
*        when 2.
*          "raise error_no_gui.
*        when 3.
*          "raise wrong_parameter.
*        when 4.
*          "raise not_supported_by_gui.
*      endcase.
    endif.
    if re_result is initial.
      raise exception type lcx_msg.
    endif.
*
  endmethod.
  method import_folder.
    data: it_files type table of  sdokpath,
          it_dir type table of sdokpath,
          lv_path   type string,
          lv_file   type ibipparms-path,
          lv_count type i.

    if i_path is initial.
      lv_path = gv_path.
    else.
      lv_path = i_path.
    endif.

    cl_gui_frontend_services=>directory_list_files(
      exporting
        directory                   = lv_path
        filter                      = '*.XML'
        files_only                  = abap_true
*        directories_only            =
      changing
        file_table                  = it_files
        count                       = lv_count
      exceptions
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        others                      = 6
    ).

      if sy-subrc <> 0.
         message | Erro ao importar o { lv_path } | type 'E'.
      else.
      "verifica se os arquivos existem
        if lv_count > 0 .
        "data vl_path type ibipparms-path.
        "executa a importação para cada arquivo encontrado
        loop at it_files into data(wa_files).
          lv_file = |{ lv_path }{ wa_files-pathname }|.
          me->import_file( i_path = lv_file ).
        endloop.
        endif.
      endif.

  endmethod.
  method import_file.
    data:  v_subrc       type sysubrc.
     data:
      "v_fieldname type dynfnam,
      v_index     type sy-index,

     " cl_xml_doc    type ref to cl_xml_document,
      "v_subrc       type sysubrc,
      v_node        type ref to if_ixml_node,
      v_iterator    type ref to if_ixml_node_iterator,
      v_nodemap     type ref to if_ixml_named_node_map,
      v_count       type i,
      v_attr        type ref to if_ixml_node,
      v_parent      type ref to if_ixml_node,
      v_name        type string,
      v_name_p      type string,
      v_value       type string,
      v_char        type char2.



    if r_xml_doc is not initial.
          call method r_xml_doc->free.
          free r_xml_doc.
        endif.
        create object r_xml_doc.
*       Import File.
        call method r_xml_doc->import_from_file
          exporting
            filename = i_path
          receiving
            retcode  = v_subrc.

        if v_subrc ne 0.
          case v_subrc.
            when '2'.
*              RAISE file_open_error.
             " message e000 with text-e03.
            when '3'.
*              RAISE file_read_error.
             " message e000 with text-e04.
            when '4'.
*              RAISE invalid_type.
              "message e000 with text-e06.
            when '5'.
*              RAISE no_batch.
              "message e000 with text-e07.
            when '6'.
*              RAISE gui_refuse_filetransfer.
              "message e000 with text-e08.
            when '99'.
*              RAISE others.
              "message e000 with text-e04.
          endcase.
        endif.

     clear it_return.

    v_node = r_xml_doc->m_document.

    check not v_node is initial.

    v_iterator = v_node->create_iterator( ).
    v_node = v_iterator->get_next( ).

    while not v_node is initial.

      case v_node->get_type( ).

        when if_ixml_node=>co_node_element.

          v_name = v_node->get_name( ).
          v_nodemap = v_node->get_attributes( ).

          v_parent = v_node.
          while not v_parent is initial.
            v_parent = v_parent->get_parent( ).
            if sy-index = 2. exit. endif.
            v_name_p = v_parent->get_name( ).

            if v_name_p eq 'emit'   or v_name_p eq 'rem'  or v_name_p eq 'exped' or
               v_name_p eq 'receb'  or v_name_p eq 'dest' or v_name_p eq 'entrega'.
              exit.
            endif.

          endwhile.

          if v_parent is initial.
            clear v_name_p.
          else.
            v_name_p = v_parent->get_name( ).
          endif.

          if v_nodemap is not initial.
*           Attributes
            v_count = v_nodemap->get_length( ).

            do v_count times.

              v_index = sy-index - 1.
              v_attr = v_nodemap->get_item( v_index ).
              v_name = v_attr->get_name( ).
              v_value = v_attr->get_value( ).

              if v_name eq 'nItem'.

                v_name_p = v_parent->get_name( ).
                st_return-tag    = v_name.
                st_return-value  = v_value.
                st_return-parent = v_name_p.

                append  st_return to it_return.
                clear   st_return.
              endif.

              if v_name eq 'versao'.

                st_return-tag    = v_name.
                st_return-value  = v_value.

                append  st_return to it_return.
                clear   st_return.
              endif.

            enddo.

          endif.

        when if_ixml_node=>co_node_text or
             if_ixml_node=>co_node_cdata_section.

*         Text Node
          v_value = v_node->get_value( ).
          move v_value to v_char.

          if v_char <> cl_abap_char_utilities=>cr_lf.

            st_return-tag    = v_name.
            st_return-value  = v_value.
            st_return-parent = v_name_p.

            append st_return to it_return.
            clear st_return.

          endif.

      endcase.
*     Advance to the next node XML
      v_node = v_iterator->get_next( ).

    endwhile.
"break-point.
    "Atribui campos do XML

    try.
      wa_cte-icon = icon_led_inactive.
      wa_cte-access_key =  it_return[ tag = 'chCTe' ]-value .
      wa_cte-xmlvers =  it_return[ tag = 'versao' ]-value .
      wa_cte-model = it_return[ tag = 'mod' ]-value .
      wa_cte-cfop_10 = it_return[ tag = 'CFOP' ]-value   .
      wa_cte-emit_cnpj = it_return[ parent = 'emit' tag = 'CNPJ'  ]-value .
      wa_cte-rem_cnpj = it_return[ parent = 'rem' tag = 'CNPJ'  ]-value .
      wa_cte-emit_xnome = it_return[ parent = 'emit' tag = 'xNome' ]-value.
      wa_cte-rem_xnome = it_return[ parent = 'rem' tag = 'xNome' ]-value.
      wa_cte-nfenum = it_return[ parent = 'infCte' tag = 'nCT' ]-value.
      wa_cte-series = it_return[ parent = 'infCte' tag = 'serie' ]-value.
      wa_cte-docdat = |{ it_return[ parent = 'infCte' tag = 'dhEmi' ]-value+8(2) }{ it_return[ parent = 'infCte' tag = 'dhEmi' ]-value+5(2) }{ it_return[ parent = 'infCte' tag = 'dhEmi' ]-value(4) }| .
      wa_cte-netpr = it_return[ parent = 'infCte' tag = 'vTPrest' ]-value.
      wa_cte-cte_strt_lct = |{ it_return[ parent = 'infCte' tag = 'UFIni' ]-value } { it_return[ parent = 'infCte' tag = 'cMunIni' ]-value }|.
      wa_cte-cte_end_lct = |{ it_return[ parent = 'infCte' tag = 'UFFim' ]-value } { it_return[ parent = 'infCte' tag = 'cMunFim' ]-value }|.
      wa_cte-authcod = it_return[ parent = 'protCTe' tag = 'nProt' ]-value.
    catch cx_sy_itab_line_not_found.

    endtry.
    "Campos facultativos no XML
    try.
      wa_cte-rate =  it_return[ parent = 'ICMS' tag = 'pICMS' ]-value.
      wa_cte-taxval = it_return[ parent = 'ICMS' tag = 'vICMS' ]-value.
      wa_cte-base = it_return[ parent = 'ICMS' tag = 'vBC' ]-value.
      catch cx_sy_itab_line_not_found.
    endtry.

     try.
      wa_cte-docnum = get_docnum( chcte = wa_alv-access_key ).
      wa_cte-bukrs = get_werks( i_cnpj = wa_alv-rem_cnpj  )-bukrs.
      wa_cte-branch = get_werks( i_cnpj = wa_alv-rem_cnpj )-j_1bbranch.
      wa_cte-werks = get_werks( i_cnpj = wa_alv-rem_cnpj  )-werks.
      wa_cte-parid = me->get_lifnr( i_cnpj = wa_alv-rem_cnpj ).
      "atribuir demais campos"""""buscar da tabela depara
    catch cx_sy_itab_line_not_found.

    endtry.

      " Dados de impostos do Cte
      set_ctestx( i_cte =  wa_cte ).

      "Valida dados e verifica
      wa_cte-icon = set_log( i_cte = wa_cte ) .



    append wa_cte to gt_cte.





    " Dados de Notas fiscais atribuidas ao CTE.
    clear wa_ctenfe.
    try.
       wa_ctenfe-chcte = it_return[ parent = 'protCTe' tag = 'chCTe' ]-value.
    catch cx_sy_itab_line_not_found.

    endtry.
    loop at it_return into data(wa_return) where parent = 'infDoc' and tag = 'chave'.
      wa_ctenfe-chnfe = wa_return-value.
      wa_ctenfe-mandt = sy-mandt.
      wa_ctenfe-counter = '001'.
      wa_ctenfe-docval = '1'."Buscar valor da respectiva nova fiscal
      wa_ctenfe-regio = wa_return-value(2) .
      wa_ctenfe-nfyear = wa_return-value+2(2) .
      wa_ctenfe-nfmonth = wa_return-value+4(2) .
      wa_ctenfe-stcd1 = wa_return-value+6(14) .
      wa_ctenfe-model = wa_return-value+20(2) .
      wa_ctenfe-series = wa_return-value+22(3) .
      wa_ctenfe-nfnum9 = wa_return-value+25(9) .
      wa_ctenfe-docnum9 = wa_return-value+35(8) .
      wa_ctenfe-cdv = wa_return-value+43(1) .

      if wa_ctenfe is not initial.
         append wa_ctenfe to gt_ctenfe.
      endif.
    endloop.

    "reak-point.

  endmethod.

  method get_depara.
    "Seleciona os dados da tabela de de-para
  " Select * from depara into table gt_depara


  endmethod.

  method get_all_werks.
   "Seleciona dados de todas as filiais - retornando centro, filial, empresa e cnpj
   select t001w~werks t001w~j_1bbranch t001k~bukrs t001k~bwkey
   from ( t001w inner join t001k on  t001k~bwkey = t001w~bwkey )
   into corresponding fields of table gt_werks .


    loop at gt_werks assigning field-symbol(<fs_werks>).

      call function 'J_1BREAD_PLANT_DATA'
        exporting
          plant                   = <fs_werks>-werks
       importing
*         ADDRESS                 =
*         BRANCH_DATA             =
         cgc_number              = <fs_werks>-stcd1
       exceptions
         plant_not_found         = 1
         branch_not_found        = 2
         address_not_found       = 3
         company_not_found       = 4
         others                  = 5
                .
      if sy-subrc eq 0.

      endif.

    endloop.

  endmethod.
  method get_werks.
    try.
      re_werks = corresponding #( gt_werks[ stcd1  = i_cnpj ] ).
    catch cx_sy_itab_line_not_found.
    endtry.
  endmethod.

  method get_lifnr.

    select single lifnr from lfa1
      into re_lifnr
      where stcd1 eq i_cnpj.

  endmethod.

  method get_docnum.

    select single docnum from j_1bnfe_active
      into re_docnum
      where regio = chcte(2)
        and nfyear = chcte+2(2)
        and nfmonth = chcte+4(2)
        and stcd1 = chcte+6(14)
        and model = chcte+20(2)
        and serie = chcte+22(3)
        and nfnum9 = chcte+25(9)
        and docnum9 = chcte+35(8)
        and cdv =  chcte+43(1)
        and cancel ne 'X'.



  endmethod.

  method set_ctestx.
    data: wa_ctestx type ty_cte_stx,
          lt_depara type table of ty_depara,
          wa_depara type ty_depara.


    append value #( werks = '1710'
                    ycfopde = '6352'
                    ycfoppara = '1352/AA'
                    transportadora = 500010
                    matnr = '801146'
                    nftype = 'YM'
                    taxlw1 = 'IC4'
                    tpicms = 'CIC2'
                    monticms = ''
                    mbeicms  = 100
                    obicms = 0
                    taxlw2 = 'I49'
                    tpipi = 'IPI2'
                    obipi = 100
                    taxlw5 = 'P01'
                    tppis = 'IPIS'
                    montpis = 'X'
                    txpis = '1.65'
                    mbepis = 0
                    obpis  = 0
                    taxlw4 = 'C01'
                    tpcofins = 'ICOF'
                    montcofins = 'X'
                    txcofins = '7.60'
                    mbecofins = 0
                    obcofins = 0
                   ) to lt_depara.

      append value #( werks = ''
                    ycfopde = '5353'
                    ycfoppara = '1352/AA'
                    transportadora = ''
                    matnr = '801146'
                    nftype = 'YM'
                    taxlw1 = 'IC0'
                    tpicms = 'CI01'
                    monticms = 'X'
                    mbeicms  = 0
                    obicms = 0
                    taxlw2 = 'I49'
                    tpipi = 'IPI2'
                    obipi = 100
                    taxlw5 = 'P01'
                    tppis = 'IPIS'
                    montpis = 'X'
                    txpis = '1.65'
                    mbepis = 0
                    obpis  = 0
                    taxlw4 = 'C01'
                    tpcofins = 'ICOF'
                    montcofins = 'X'
                    txcofins = '7.60'
                    mbecofins = 0
                    obcofins = 0
                   ) to lt_depara.



    wa_ctestx-chcte = i_cte-access_key.
    wa_ctestx-itmnum = '000010'.

    "Dados de tributação
    try.
      wa_depara = corresponding #( lt_depara[ werks = i_cte-werks ycfopde = i_cte-cfop_10 transportadora = i_cte-parid  ] ).
      catch cx_sy_itab_line_not_found.
        try.
          wa_depara = corresponding #( lt_depara[ werks = i_cte-werks ycfopde = i_cte-cfop_10 transportadora = i_cte-parid  ] ).
        catch cx_sy_itab_line_not_found.
          "Adiciona mensagem de erro para o cte - Dados da YTR009/Ydtr0010 faltando
        endtry.
    endtry.

      wa_ctestx-taxtyp = wa_depara-tpicms.
      wa_ctestx-taxval = ( i_cte-netpr * i_cte-rate ) / 100.
      wa_ctestx-rate = i_cte-rate.
      if wa_depara-monticms = abap_true.
        wa_ctestx-base =  i_cte-netpr.
        wa_ctestx-execbas = 0.
        wa_ctestx-othbas = 0.
      endif.
      if wa_depara-mbeicms = 100.
           wa_ctestx-base =  0.
           wa_ctestx-execbas = i_cte-netpr.
           wa_ctestx-othbas = 0.
      endif.
      if wa_depara-obicms = 100.
           wa_ctestx-base =  0.
           wa_ctestx-execbas = 0.
           wa_ctestx-othbas = i_cte-netpr.
       endif.

      append wa_ctestx to gt_ctestx.
      clear: wa_ctestx-taxtyp, wa_ctestx-base, wa_ctestx-othbas, wa_ctestx-execbas, wa_ctestx-rate, wa_ctestx-taxval.



    "Dados IPI
    wa_ctestx-taxtyp = wa_depara-tpipi.
    wa_ctestx-base = 0.
    wa_ctestx-othbas = i_cte-netpr.
    wa_ctestx-execbas = 0.
    wa_ctestx-rate = 0.
    wa_ctestx-taxval = 0.
    append wa_ctestx to gt_ctestx.
    clear: wa_ctestx-taxtyp, wa_ctestx-base, wa_ctestx-othbas, wa_ctestx-execbas, wa_ctestx-rate, wa_ctestx-taxval.

    "Dados PIS
    wa_ctestx-taxtyp = wa_depara-tpipi.
    wa_ctestx-rate = wa_depara-txpis.
    wa_ctestx-taxval = ( wa_depara-txpis * i_cte-netpr ) / 100.
    if wa_depara-montpis = abap_true.
       wa_ctestx-base =  i_cte-netpr.
       wa_ctestx-othbas = 0.
       wa_ctestx-execbas = 0.
    endif.
    if wa_depara-obpis = 100.
       wa_ctestx-base =  0.
       wa_ctestx-othbas = i_cte-netpr.
       wa_ctestx-execbas = 0.
    endif.



    append wa_ctestx to gt_ctestx.
    clear: wa_ctestx-taxtyp, wa_ctestx-base, wa_ctestx-othbas, wa_ctestx-execbas, wa_ctestx-rate, wa_ctestx-taxval.

    "Dados COFINS
    wa_ctestx-taxtyp = wa_depara-tpcofins.
    wa_ctestx-rate = wa_depara-txcofins.
    wa_ctestx-taxval = ( wa_depara-txcofins * i_cte-netpr ) / 100.
    if wa_depara-montcofins = abap_true.
       wa_ctestx-base =  i_cte-netpr.
       wa_ctestx-othbas = 0.
       wa_ctestx-execbas = 0.
    endif.
    if wa_depara-obcofins = 100.
       wa_ctestx-base =  0.
       wa_ctestx-othbas = i_cte-netpr.
       wa_ctestx-execbas = 0.
    endif.

    append wa_ctestx to gt_ctestx.
    clear: wa_ctestx-taxtyp, wa_ctestx-base, wa_ctestx-othbas, wa_ctestx-execbas, wa_ctestx-rate, wa_ctestx-taxval.

    clear wa_ctestx.
  endmethod.
  method set_log.
    data lv_msg type bal_s_msg.


    if i_cte-docnum is not initial.
      re_icon = icon_led_green.
      lv_msg-msgid = 'ZMSG'.
      lv_msg-msgno = '002' .
      lv_msg-msgty = 'S'.
      lv_msg-msgv1 = i_cte-access_key.
      lv_msg-msgv2 = 'CTe ja registrado'.
    endif.
    if i_cte-parid is initial.
      re_icon = icon_led_red.
      lv_msg-msgid = 'ZMSG'.
      lv_msg-msgno = '002' .
      lv_msg-msgty = 'E'.
      lv_msg-msgv1 = i_cte-access_key.
      lv_msg-msgv2 = 'Transportador não encontrado'.
      me->add_log( i_msg = lv_msg ).
      clear lv_msg.
    endif.
    if i_cte-bukrs is initial or i_cte-werks is initial or i_cte-branch is initial.
      lv_msg-msgid = 'ZMSG'.
      lv_msg-msgno = '002' .
      lv_msg-msgty = 'E'.
      lv_msg-msgv1 = i_cte-access_key.
      lv_msg-msgv2 = 'Dados de Empresa/Filial/Centro não encontrado!'.
      re_icon = icon_led_red.
      me->add_log( i_msg = lv_msg ).
      clear lv_msg.
    endif.

  endmethod.

  method create_log.
    data: lst_log type bal_s_log.

* Defining some header data of the application log.
  lst_log-extnumber = sy-uzeit.   " Text: 'Program Log'.
  lst_log-aluser    = sy-uname.
  lst_log-alprog    = sy-repid.

* Creationg the application log.
  call function 'BAL_LOG_CREATE'
    exporting
      i_s_log = lst_log
    exceptions
      others  = 1.
  if sy-subrc ne 0.

*   Display error message.


  endif.

  endmethod.
  method add_log.
    data:  lst_msg type bal_s_msg.

* Defininig data of message for the application log.
  lst_msg-msgty     = i_msg-msgty.
  lst_msg-msgid     = i_msg-msgid.
  lst_msg-msgno     = i_msg-msgno.
  lst_msg-msgv1     = i_msg-msgv1.
  lst_msg-msgv2     = i_msg-msgv2.
  lst_msg-msgv3     = i_msg-msgv3.
  lst_msg-msgv4     = i_msg-msgv4.
  lst_msg-probclass = 3.

* Adding this message to log file.
  call function 'BAL_LOG_MSG_ADD'
    exporting
      i_s_msg       = lst_msg
    exceptions
      log_not_found = 0
      others        = 1.
  if sy-subrc ne 0.



  endif.


  endmethod.
  method show_log.

  call function 'BAL_DSP_LOG_DISPLAY'
    exporting
*     Sort logs by Timestamp ('X') or Log Number (SPACE).
      i_srt_by_timstmp     = space
    exceptions
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4.
  if sy-subrc ne 0.

  endif.

  endmethod.
  method process_cte.

   data: ls_header type bapi_cte_j_1bnfdoc,
         ls_nfcheck type bapi_cte_j_1bnfcheck,
         ls_docnum type bapi_cte_j_1bnfdoc-docnum,
         lt_item type table of bapi_cte_j_1bnflin,
         ls_item type bapi_cte_j_1bnflin,
         lt_item_tax type table of bapi_cte_j_1bnfstx,
         ls_item_tax type  bapi_cte_j_1bnfstx,
         lt_cte_docref type  table of bapi_j_1bcte_d_docref,
         ls_cte_docref type bapi_j_1bcte_d_docref,
         lt_return type table of bapiret2,
         lt_selection type salv_t_row,
         lt_cte_sel type table of ty_cte,
         lv_msg type bal_s_msg.



  "r_model->get_data_by_index( i_selection =  r_view->r_alv->get_selections( )->get_selected_rows( ) ).
  lt_selection = me->r_alv->get_selections( )->get_selected_rows( ).

  loop at lt_selection into data(index).
       try.
         append gt_cte[ index ] to lt_cte_sel.
         "gt_alv_index = CORRESPONDING #( gt_alv[ index ] ).
       catch cx_root.
       endtry.
   endloop.
   if not line_exists( lt_cte_sel[ 1 ] ) .
     message | selecione um ou mais registro  { sy-datum } |  type 'I'.
     exit.
   endif.
   "break-point.

  loop at lt_cte_sel assigning field-symbol(<fs_cte>).
      clear: ls_header, ls_nfcheck,ls_item,ls_item_tax, lt_cte_docref, lt_return.

      ls_header = corresponding #( <fs_cte> ).
      ls_nfcheck = corresponding #( <fs_cte> ).

      ls_item = corresponding #( <fs_cte> ).
      append ls_item to lt_item.
      clear ls_item.
      "monta tabela de impostos

      loop at gt_ctestx into data(ls_cte_stx) where chcte = <fs_cte>-access_key.
         ls_item_tax  = corresponding #( ls_cte_stx ).
         append  ls_item_tax to lt_item_tax.
         clear ls_item_tax.
      endloop.

      "monta tabela de notas fiscais por Cte
      loop at gt_ctenfe into data(ls_cte_nfe) where chcte = <fs_cte>-access_key.
         ls_cte_docref = corresponding #( ls_cte_nfe ).
         append ls_cte_docref to lt_cte_docref.
         clear: ls_cte_docref.
      endloop.

   call function 'BAPI_CTE_J_1B_NF_CREATFROMDATA'
     exporting
       is_header           = ls_header
       is_nfcheck          = ls_nfcheck
    importing
      ev_docnum           = ls_docnum
     tables
*      O_PARTNER           =
       o_item              = lt_item
*      O_ITEM_ADD          =
       o_item_tax          = lt_item_tax
*      O_HEADER_MSG        =
*      O_REFER_MSG         =
*      O_OT_PARTNER        =
*      O_IMPORT_DI         =
*      O_IMPORT_ADI        =
       o_cte_docref        = lt_cte_docref
*      O_CTE_RES           =
*      O_EXPORT            =
       return              = lt_return
             .

    "  BREAK-POINT.
      if ls_docnum is INITIAL.
        loop at lt_return into data(ls_return) .
            lv_msg-msgty = 'E'.
          lv_msg-msgv1 = ls_header-access_key && '-' && ls_return-message.
          me->add_log( i_msg = lv_msg ).
        endloop.
      else.
         data(wl_cte) = gt_cte[   access_key  = <fs_Cte>-access_key ].
         "refresh dados do ALV para o CTE.
         wl_cte-icon = ICON_LED_GREEN.
         wl_cte-docnum = ls_docnum.
         MODIFY gt_cte from wl_cte TRANSPORTING icon docnum where access_key = <fs_Cte>-access_key.
         r_alv->refresh( ).
      endif.

   endloop.
  endmethod.

  method view_alv.

    data: r_events    type ref to cl_salv_events_table,
          r_selections type ref to cl_salv_selections.

    try.
      cl_salv_table=>factory(
    exporting
       list_display   = if_salv_c_bool_sap=>false
    "r_container    =
    "container_name = 'Name'
     importing
       r_salv_table   = r_alv
     changing
       t_table        = gt_cte ).


  r_alv->set_screen_status(
    pfstatus      =  'STATUS_GUI_1'
    report        =  sy-repid
    set_functions = r_alv->c_functions_all ).



  catch cx_salv_msg.

  endtry.
  r_selections = r_alv->get_selections( ).
  r_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  r_events = r_alv->get_event( ).
  "set handler on_link_click for r_events.
  set handler on_user_command for r_events.

  r_alv->display( ).
  endmethod.

  method on_user_command.
     case e_salv_function.
     when '&ESCRITURA'.
       me->process_cte( ).

     when '&LOG'.
       me->show_log( ).

   endcase.
  endmethod.

  method movefile.

    data : lv_dir_exist type abap_bool,
           lv_path type string,
           lv_file type string.

    lv_path = gv_path.

    cl_gui_frontend_services=>file_copy(
      exporting
        source               = lv_path && lv_file
        destination          = lv_path && 'Processados\' && lv_file
        overwrite            = space
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        disk_full            = 4
        access_denied        = 5
        file_not_found       = 6
        destination_exists   = 7
        unknown_error        = 8
        path_not_found       = 9
        disk_write_protect   = 10
        drive_not_ready      = 11
        not_supported_by_gui = 12
        others               = 13
    ).
    if sy-subrc <> 0.
     message id sy-msgid type sy-msgty number sy-msgno
       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
*
*    CL_GUI_FRONTEND_SERVICES=>file_exist(
*      exporting
*        file                 =
**      receiving
**        result               =
**      exceptions
**        cntl_error           = 1
**        error_no_gui         = 2
**        wrong_parameter      = 3
**        not_supported_by_gui = 4
**        others               = 5
*    ).
*    if sy-subrc <> 0.
**     message id sy-msgid type sy-msgty number sy-msgno
**       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    else.
*
*    CL_GUI_FRONTEND_SERVICES=>file_delete(
*      exporting
*        filename             =
*      changing
*        rc                   =
**      exceptions
**        file_delete_failed   = 1
**        cntl_error           = 2
**        error_no_gui         = 3
**        file_not_found       = 4
**        access_denied        = 5
**        unknown_error        = 6
**        not_supported_by_gui = 7
**        wrong_parameter      = 8
**        others               = 9
*    ).
*    if sy-subrc <> 0.
**     message id sy-msgid type sy-msgty number sy-msgno
**       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    endif.
*    endif.
  endmethod.
endclass.




data r_app type ref to lcl_app.

selection-screen begin of block b1 with frame title text-t01.

parameters: p_path type ibipparms-path.

selection-screen skip 1.

parameters: rb_loc radiobutton group rb01,
            rb_ser  radiobutton group rb01,
            rb_lote radiobutton group rb01.

selection-screen end of block b1.


************************************************************************
*    A T   S E L E C T I O N - S C R E E N   O N  V A L U E  R E Q.    *
************************************************************************
at selection-screen on value-request for p_path.

* F4 for filename / Filemanager support to locate file in a directory
  call function 'F4_FILENAME'
    exporting
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = lcl_app=>v_fieldname
    importing
      file_name     = p_path.

************************************************************************
*              S T A R T - O F - S E L E C T I O N                     *
************************************************************************
start-of-selection.


  r_app = new lcl_app( ).
  r_app->create_log( ).
  r_app->gv_path = p_path.
  r_app->get_all_werks( ).
  r_app->check_folder_process( ). "verificar se existe a pasta Processados no caminho, senao a cria.



  if rb_loc is not initial. "Arquivo
     try.
        data(vl_error) = r_app->check_file_path( ).
        r_app->import_file( i_path =  r_app->gv_path ).
     catch lcx_msg.
        message 'Erro ao ler arquivo' type 'I'.
     endtry.

*
  elseif rb_ser is not initial. "Pasta

   try.
      data(vl_error1) = r_app->check_folder_path( ).
      r_app->import_folder(  ).
   catch lcx_msg.
       message 'Erro ao ler o diretório.' type 'I'.
   endtry.


  endif.
  r_app->view_alv( ).