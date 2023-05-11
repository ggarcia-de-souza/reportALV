*&---------------------------------------------------------------------*
*& Report z_rxnguilherme_alv_git
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_rxnguilherme_alv_git.

* TABELA

TABLES: SFLIGHT,
        SPFLI,
        SCARR.


* CLASSE

CLASS LCL_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS: HOTSPOT_CLICK FOR EVENT LINK_CLICK OF
      CL_SALV_EVENTS_TABLE
      IMPORTING
        ROW
        COLUMN.
ENDCLASS.
CLASS LCL_EVENTS IMPLEMENTATION.
  METHOD HOTSPOT_CLICK.
    PERFORM F_HOTSPOT USING ROW
                            COLUMN.
  ENDMETHOD.
ENDCLASS.


* TIPOS

TYPES: BEGIN OF TY_SAIDA,

         CARRID    LIKE SFLIGHT-CARRID,
         CONNID    LIKE SFLIGHT-CONNID,
         COUNTRYFR LIKE SPFLI-COUNTRYFR,
         CITYFROM  LIKE SPFLI-CITYFROM,
         AIRPFROM  LIKE SPFLI-AIRPFROM,
         COUNTRYTO LIKE SPFLI-COUNTRYTO,
         DISTANCE  LIKE SPFLI-DISTANCE,
         DISTID    TYPE STRING,
         FLDATE    LIKE SFLIGHT-FLDATE,
         PRICE     LIKE SFLIGHT-PRICE,

       END OF TY_SAIDA.

* TABELAS INTERNAS
DATA: T_TESTE TYPE TABLE OF SPFLI.
DATA: T_SFLIGHT TYPE TABLE OF SFLIGHT.
DATA: T_SCARR TYPE TABLE OF SCARR.
DATA: T_SPFLI TYPE TABLE OF SPFLI.
DATA: T_SAIDA TYPE TABLE OF TY_SAIDA.
DATA: T_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA: T_HEADER TYPE SLIS_T_LISTHEADER.

*DATA: BEGIN OF T_STRUCTURE OCCURS 0,
*
*        CARRID    LIKE SFLIGHT-CARRID,
*        PRICE     LIKE SFLIGHT-PRICE,
*        COUNTRYFR LIKE SPFLI-COUNTRYFR,
*        CITYFROM  LIKE SPFLI-CITYFROM,
*        AIRPFROM  LIKE SPFLI-AIRPFROM,
*        COUNTRYTO LIKE SPFLI-COUNTRYTO,
*
*      END OF T_STRUCTURE.
*
DATA: GR_TABLE  TYPE REF TO CL_SALV_TABLE.

* WORK AREAS

DATA: W_SFLIGHT TYPE SFLIGHT.
DATA: W_SPFLI TYPE SPFLI.
DATA: W_SAIDA TYPE TY_SAIDA.
DATA: W_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
DATA: W_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA: W_HEADER TYPE SLIS_LISTHEADER.

* VARIAVEL

DATA: LV_DISTANCE TYPE STRING.



* TELA DE SELEÇÃO

SELECTION-SCREEN BEGIN OF BLOCK BC01 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: S_CARRID FOR SFLIGHT-CARRID.

SELECTION-SCREEN END OF BLOCK BC01.

* P_VARIAN SERVE PARA GRAVAR VARIANTE DO ALV

SELECTION-SCREEN BEGIN OF BLOCK BC02 WITH FRAME TITLE TEXT-002.

  PARAMETERS: P_VARIAN TYPE SLIS_VARI.

SELECTION-SCREEN END OF BLOCK BC02.

PARAMETERS P_CHKBOX AS CHECKBOX DEFAULT ''.

AT SELECTION-SCREEN.

  IF S_CARRID[] IS INITIAL.

    MESSAGE 'Não pode estar em branco' TYPE 'E'.

  ENDIF.

START-OF-SELECTION.

  PERFORM F_SELECIONA_DADOS.

  PERFORM F_MONTA_SAIDA.

  PERFORM F_MONTA_ALV.

*  PERFORM .
*&---------------------------------------------------------------------*
*& Form F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

*  SELECT * FROM SFLIGHT INTO TABLE T_SFLIGHT
*    WHERE CARRID IN S_CARRID.
*
*  IF SY-SUBRC IS INITIAL.
*
*    SELECT * FROM SPFLI INTO TABLE T_SPFLI
*      FOR ALL ENTRIES IN T_SFLIGHT
*      WHERE CARRID = T_SFLIGHT-CARRID.
*
*  ELSE.
*    MESSAGE TEXT-003 TYPE 'I'.
*    STOP.

  SELECT MANDT CARRID FROM SCARR INTO TABLE T_SCARR
    WHERE CARRID IN S_CARRID.

  SORT T_SCARR BY CARRID.

  IF SY-SUBRC IS INITIAL.

    SELECT * FROM SPFLI INTO TABLE T_SPFLI
      FOR ALL ENTRIES IN T_SCARR
      WHERE CARRID = T_SCARR-CARRID.

    SELECT * FROM SFLIGHT INTO TABLE T_SFLIGHT
      FOR ALL ENTRIES IN T_SPFLI
      WHERE CONNID = T_SPFLI-CONNID.


  ELSE.
    MESSAGE TEXT-003 TYPE 'I'.
    STOP.

  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_MONTA_SAIDA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_MONTA_SAIDA .


*           CARRID    LIKE SFLIGHT-CARRID,
*         CONNID    LIKE SFLIGHT-CONNID,
*         COUNTRYFR LIKE SPFLI-COUNTRYFR,
*         CITYFROM  LIKE SPFLI-CITYFROM,
*         AIRPFROM  LIKE SPFLI-AIRPFROM,
*         COUNTRYTO LIKE SPFLI-COUNTRYTO,
*         DISTANCE  LIKE SPFLI-DISTANCE,
*         DISTID    TYPE STRING,
*         FLDATE    LIKE SFLIGHT-FLDATE,
*         PRICE     LIKE SFLIGHT-PRICE,

*  LOOP AT T_SFLIGHT INTO W_SFLIGHT.
*
*    CLEAR W_SPFLI.
*
*    W_SAIDA-CARRID    = W_SFLIGHT-CARRID.
*    W_SAIDA-PRICE     = W_SFLIGHT-PRICE.
*
*    READ TABLE T_SPFLI INTO W_SPFLI WITH KEY CARRID = W_SFLIGHT-CARRID.
*
*    IF SY-SUBRC IS INITIAL.
*      W_SAIDA-COUNTRYFR  = W_SPFLI-COUNTRYFR.
*      W_SAIDA-CITYFROM = W_SPFLI-CITYFROM.
*      W_SAIDA-AIRPFROM = W_SPFLI-AIRPFROM.
*      W_SAIDA-COUNTRYTO = W_SPFLI-COUNTRYTO.
*
*    ENDIF.
*
*    APPEND W_SAIDA TO T_SAIDA.
*
*
*  ENDLOOP.

  LOOP AT T_SFLIGHT INTO W_SFLIGHT.

    W_SAIDA-FLDATE = W_SFLIGHT-FLDATE.
    W_SAIDA-PRICE = W_SFLIGHT-PRICE.

    READ TABLE T_SPFLI INTO W_SPFLI WITH KEY CONNID = W_SFLIGHT-CONNID.

* IF para passar pro próximo item
    IF W_SPFLI-CARRID = 'AA'.

      CONTINUE.

    ENDIF.

    W_SAIDA-CARRID = W_SPFLI-CARRID.
    W_SAIDA-CONNID = W_SPFLI-CONNID.
    W_SAIDA-COUNTRYFR  = W_SPFLI-COUNTRYFR.
    W_SAIDA-CITYFROM = W_SPFLI-CITYFROM.
    W_SAIDA-AIRPFROM = W_SPFLI-AIRPFROM.
    W_SAIDA-COUNTRYTO = W_SPFLI-COUNTRYTO.

    MOVE W_SPFLI-DISTANCE TO LV_DISTANCE.

    CONCATENATE LV_DISTANCE W_SPFLI-DISTID INTO W_SAIDA-DISTID.

    APPEND W_SAIDA TO T_SAIDA.

  ENDLOOP.

*  LOOP AT T_SFLIGHT INTO W_SFLIGHT.
*
*    W_SAIDA-FLDATE = W_SFLIGHT-FLDATE.
*    W_SAIDA-PRICE = W_SFLIGHT-PRICE.
*
*    READ TABLE T_SPFLI INTO W_SPFLI WITH KEY CONNID = W_SFLIGHT-CONNID.
*
*    W_SAIDA-CARRID = W_SPFLI-CARRID.
*    W_SAIDA-CONNID = W_SPFLI-CONNID.
*    W_SAIDA-COUNTRYFR  = W_SPFLI-COUNTRYFR.
*    W_SAIDA-CITYFROM = W_SPFLI-CITYFROM.
*    W_SAIDA-AIRPFROM = W_SPFLI-AIRPFROM.
*    W_SAIDA-COUNTRYTO = W_SPFLI-COUNTRYTO.
*
*    MOVE W_SPFLI-DISTANCE TO LV_DISTANCE.
*
*    CONCATENATE LV_DISTANCE W_SPFLI-DISTID INTO W_SAIDA-DISTID.
*
*    APPEND W_SAIDA TO T_SAIDA.
*
*  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MONTA_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_MONTA_ALV .

  PERFORM F_DEFINE_FIELDCAT.

  PERFORM F_LAYOUT.

  PERFORM F_IMPRIME_ALV.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DEFINE_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_DEFINE_FIELDCAT .

* Esse método de construir um FIELDCAT só funciona se vc utilizar um estrutura que foi criada na SE11

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      I_PROGRAM_NAME         = SY-REPID
*      I_INTERNAL_TABNAME     = 'T_SAIDA'
*      I_STRUCTURE_NAME       = 'SPFLI'
**     I_CLIENT_NEVER_DISPLAY = 'X'
**     I_INCLNAME             =
**     I_BYPASSING_BUFFER     =
**     I_BUFFER_ACTIVE        =
*    CHANGING
*      CT_FIELDCAT            = T_FIELDCAT
*    EXCEPTIONS
*      INCONSISTENT_INTERFACE = 1
*      PROGRAM_ERROR          = 2
*      OTHERS                 = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE TEXT-006 TYPE 'I'.
*    STOP.
*  ENDIF.

* Uma estrutura que está só no código fonte terá que ser feita da forma abaixo


*         CARRID    LIKE SFLIGHT-CARRID,
*         CONNID    LIKE SFLIGHT-CONNID,
*         PRICE     LIKE SFLIGHT-PRICE,
*         COUNTRYFR LIKE SPFLI-COUNTRYFR,
*         CITYFROM  LIKE SPFLI-CITYFROM,
*         AIRPFROM  LIKE SPFLI-AIRPFROM,
*         COUNTRYTO LIKE SPFLI-COUNTRYTO,
*         DISTANCE  LIKE SPFLI-DISTANCE,
*         DISTID    TYPE STRING,
*         FLDATE    LIKE SFLIGHT-FLDATE,
*         PRICE     LIKE SFLIGHT-PRICE,

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'CARRID'.
  W_FIELDCAT-SELTEXT_M = 'Comp. Aérea'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'CONNID'.
  W_FIELDCAT-SELTEXT_M = 'Cod. Conexão'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'PRICE'.
  W_FIELDCAT-SELTEXT_M = 'Preço'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'FLDATE'.
  W_FIELDCAT-SELTEXT_M = 'Data vôo'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'COUNTRYFR'.
  W_FIELDCAT-SELTEXT_M = 'País partida'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'CITYFROM'.
  W_FIELDCAT-SELTEXT_M = 'Cidade de chegada'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'AIRPFROM'.
  W_FIELDCAT-SELTEXT_M = 'Aeroporto de partida'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'COUNTRYTO'.
  W_FIELDCAT-SELTEXT_M = 'País destino'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

  W_FIELDCAT-COL_POS = '1'.
  W_FIELDCAT-ROW_POS = '1'.
  W_FIELDCAT-FIELDNAME = 'DISTID'.
  W_FIELDCAT-SELTEXT_M = 'Distância'.
  APPEND W_FIELDCAT TO T_FIELDCAT.
  CLEAR W_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_LAYOUT .

  W_LAYOUT-ZEBRA = 'X'.
  W_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_IMPRIME_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_IMPRIME_ALV .
  DATA: LO_COLS_TAB TYPE REF TO CL_SALV_COLUMNS_TABLE,
        LO_COL_TAB  TYPE REF TO CL_SALV_COLUMN_TABLE.
  DATA: LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST.
  DATA LO_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS.
  DATA: LO_HEADER  TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
        LO_H_LABEL TYPE REF TO CL_SALV_FORM_LABEL,
        LO_H_FLOW  TYPE REF TO CL_SALV_FORM_LAYOUT_FLOW.
  DATA: LO_LOGO TYPE REF TO CL_SALV_FORM_LAYOUT_LOGO.
  DATA: LS_COLOR TYPE LVC_S_COLO.
  DATA: STR TYPE STRING.


* Objetos da classe
  DATA: LO_EVENTS  TYPE REF TO CL_SALV_EVENTS_TABLE,
        LO_HANDLER TYPE REF TO LCL_EVENTS.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
**     I_INTERFACE_CHECK      = ' '
**     I_BYPASSING_BUFFER     = ' '
**     I_BUFFER_ACTIVE        = ' '
*      I_CALLBACK_PROGRAM     = SY-REPID
**     I_CALLBACK_PF_STATUS_SET          = ' '
**     I_CALLBACK_USER_COMMAND           = ' '
*      I_CALLBACK_TOP_OF_PAGE = 'F_CABECALHO'
**     I_CALLBACK_HTML_TOP_OF_PAGE       = 'TOP-OF-PAGE'
**     I_CALLBACK_HTML_END_OF_LIST       = ' '
*      I_STRUCTURE_NAME       = 'T_SAIDA'
**     I_BACKGROUND_ID        = ' '
**     I_GRID_TITLE           =
**     I_GRID_SETTINGS        =
*      IS_LAYOUT              = W_LAYOUT
*      IT_FIELDCAT            = T_FIELDCAT
**     IT_EXCLUDING           =
**     IT_SPECIAL_GROUPS      =
**     IT_SORT                =
**     IT_FILTER              =
**     IS_SEL_HIDE            =
**     I_DEFAULT              = 'X'
**     I_SAVE                 = ' '
**     IS_VARIANT             =
**     IT_EVENTS              =
**     IT_EVENT_EXIT          =
**     IS_PRINT               =
**     IS_REPREP_ID           =
**     I_SCREEN_START_COLUMN  = 0
**     I_SCREEN_START_LINE    = 0
**     I_SCREEN_END_COLUMN    = 0
**     I_SCREEN_END_LINE      = 0
**     I_HTML_HEIGHT_TOP      = 0
**     I_HTML_HEIGHT_END      = 0
**     IT_ALV_GRAPHICS        =
**     IT_HYPERLINK           =
**     IT_ADD_FIELDCAT        =
**     IT_EXCEPT_QINFO        =
**     IR_SALV_FULLSCREEN_ADAPTER        =
**     O_PREVIOUS_SRAL_HANDLER           =
** IMPORTING
**     E_EXIT_CAUSED_BY_CALLER           =
**     ES_EXIT_CAUSED_BY_USER =
*    TABLES
*      T_OUTTAB               = T_SAIDA
*    EXCEPTIONS
*      PROGRAM_ERROR          = 1
*      OTHERS                 = 2.

  TRY.

      CALL METHOD CL_SALV_TABLE=>FACTORY
        IMPORTING
          R_SALV_TABLE = GR_TABLE
        CHANGING
          T_TABLE      = T_SAIDA.

* Define cor
      LO_COLS_TAB = GR_TABLE->GET_COLUMNS( ).
      LO_COL_TAB ?= LO_COLS_TAB->GET_COLUMN( 'CARRID' ).
      LS_COLOR-COL = '3'.
      LS_COLOR-INT = '0'.
      LS_COLOR-INV = '0'.
      LO_COL_TAB->SET_COLOR( LS_COLOR ).

* Hotspot
      LO_COL_TAB->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).

* Otimiza colunas

      LO_COLS_TAB->SET_OPTIMIZE( 'X' ).

* Zebra layout

      LO_SETTINGS = GR_TABLE->GET_DISPLAY_SETTINGS( ).
      LO_SETTINGS->SET_STRIPED_PATTERN( IF_SALV_C_BOOL_SAP=>TRUE ).

* Modifica nome da coluna
      LO_COL_TAB ?= LO_COLS_TAB->GET_COLUMN( 'DISTID' ).
      LO_COL_TAB->SET_LONG_TEXT( 'Distância' ).

* Remove coluna
      LO_COL_TAB ?= LO_COLS_TAB->GET_COLUMN( 'DISTANCE' ).
      LO_COL_TAB->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).

* Cabeçalho ALV

      CREATE OBJECT LO_HEADER.

      LO_H_LABEL = LO_HEADER->CREATE_LABEL( ROW = 1 COLUMN = 1 ).
      LO_H_LABEL->SET_TEXT( 'Relatório de vôos' ).

*  LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 2  COLUMN = 1 ).
*  LO_H_FLOW->CREATE_TEXT( TEXT =  'Data.:'  ).
*
*  LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 2  COLUMN = 2 ).
*  LO_H_FLOW->CREATE_TEXT( TEXT =  SY-DATUM  ).

      LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 2  COLUMN = 1 ).
      LO_H_FLOW->CREATE_TEXT( TEXT = | { 'Data.:' }  { SY-DATUM DATE = USER } |  ).

*
*  LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 3  COLUMN = 1 ).
*  LO_H_FLOW->CREATE_TEXT( TEXT = 'Hora.:' ).
*
*  LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 3  COLUMN = 2 ).
*  LO_H_FLOW->CREATE_TEXT( TEXT = SY-UZEIT ).

      LO_H_FLOW = LO_HEADER->CREATE_FLOW( ROW = 3  COLUMN = 1 ).
      LO_H_FLOW->CREATE_TEXT( TEXT = | { 'Hora.:' }  { SY-UZEIT TIME = USER } |  ).

      CREATE OBJECT LO_LOGO.

      LO_LOGO->SET_LEFT_CONTENT( LO_HEADER ).
      LO_LOGO->SET_RIGHT_LOGO( 'ENJOYSAP_LOGO' ).

* Toolbar da ALV
      LO_FUNCTIONS = GR_TABLE->GET_FUNCTIONS( ).
      LO_FUNCTIONS->SET_DEFAULT( ABAP_TRUE ).

* Evento do hotspot

      LO_EVENTS = GR_TABLE->GET_EVENT( ).
      CREATE OBJECT LO_HANDLER.
      SET HANDLER LO_HANDLER->HOTSPOT_CLICK FOR LO_EVENTS.

* Display o ALV

      GR_TABLE->SET_TOP_OF_LIST( LO_HEADER ).
      GR_TABLE->SET_TOP_OF_LIST( LO_LOGO ).
      GR_TABLE->DISPLAY( ).

    CATCH CX_SALV_NOT_FOUND.

      MESSAGE 'ERRO' TYPE 'E'.


    CATCH CX_SALV_MSG.

      MESSAGE 'ERRO' TYPE 'E'.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CABECALHO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_CABECALHO .

*  CLEAR W_HEADER.
*  REFRESH T_HEADER.

  W_HEADER-TYP = 'H'.
  W_HEADER-INFO = 'Relatório de vôos'.
  APPEND W_HEADER TO T_HEADER.

  W_HEADER-TYP = 'S'.
  W_HEADER-INFO = 'Data.'.
  WRITE SY-DATUM TO W_HEADER-INFO.
  APPEND W_HEADER TO T_HEADER.

  W_HEADER-TYP = 'S'.
  W_HEADER-INFO = 'Hora.'.
  WRITE SY-UZEIT TO W_HEADER-INFO.
  APPEND W_HEADER TO T_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER
      I_LOGO             = 'ENJOYSAP_LOGO'
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .

ENDFORM.
FORM F_HOTSPOT USING P_ROW TYPE SALV_DE_ROW
                     P_COLUMN TYPE SALV_DE_COLUMN.

* Verificar se é a coluna clicada
  IF P_COLUMN EQ 'CARRID'.

    READ TABLE T_SAIDA INTO W_SAIDA INDEX P_ROW.

    IF SY-SUBRC EQ 0.

      READ TABLE T_SAIDA INTO W_SAIDA WITH KEY CARRID = W_SAIDA-CARRID.

*      WRITE: 'Você clicou na Companhia Aérea', W_SAIDA-CARRID.

      CL_DEMO_OUTPUT=>WRITE_TEXT( | { 'você clicou na Companhia Aérea' }  { W_SAIDA-CARRID } | ).
      CL_DEMO_OUTPUT=>DISPLAY( ).
    ENDIF.

  ENDIF.

ENDFORM.
