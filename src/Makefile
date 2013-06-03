BINDIR=../bin

OCAMLC=ocamlc
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLFIND=ocamlfind
FINDFLAGS=-g -package "threads,lablgtk2,lablgtk2.sourceview2,dynlink,compiler-libs,compiler-libs.common,compiler-libs.toplevel" -thread
GTKINIT=$(shell ocamlfind query lablgtk2)/gtkInit.cmo

LINK=-linkpkg

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) $(FINDFLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLFIND) ${OCAMLC} ${FINDFLAGS} -c $<

all: ocabug

lexer_debug.ml: lexer_debug.mll
	$(OCAMLLEX) lexer_debug.mll

parser_debug.ml parser_debug.mli: parser_debug.mly
	${OCAMLYACC} $<

clean:
	-rm -f *.cmi *.cmo
	-rm -f parser_debug.ml parser_debug.mli
	-rm -f lexer_debug.ml
	-rm -f ocabug

ocabug: $(GTKINIT) ocabug_icons.cmo ocabug_view.cmo ocabug_config.cmo ocabug_misc.cmo ocabug_event_boxes.cmo int64ops.cmo \
primitives.cmo unix_tools.cmo debugger_config.cmo envaux.cmo parameters.cmo lexer_debug.cmo input_handling.cmo question.cmo debugcom.cmo exec.cmo source.cmo pos.cmo checkpoints.cmo \
events.cmo program_loading.cmo symbols.cmo breakpoints.cmo trap_barrier.cmo history.cmo printval.cmo show_source.cmo time_travel.cmo program_management.cmo frames.cmo eval.cmo \
show_information.cmo loadprinter.cmo parser_debug.cmo command_line.cmo ocabug_controller.cmo main_debug.cmo
	$(OCAMLFIND) $(OCAMLC) $(FINDFLAGS) $(LINK) $^ -o $@

#unix_tools.cmo time_travel.cmo show_information.cmo question.cmo program_management.cmo primitives.cmo parameters.cmo ocabug_misc.cmo ocabug_controller.cmo ocabug_config.cmo input_handling.cmo frames.cmo exec.cmo debugger_config.cmo command_line.cmo checkpoints.cmo main_debug.cmo
lexer_debug.ml: parser_debug.cmi lexer_debug.mll
lexer_debug.mli: parser_debug.cmi
lexer_debug.cmo: parser_debug.cmi lexer_debug.cmi
lexer_debug.cmx: parser_debug.cmx lexer_debug.cmi

parser_debug.cmi: parser_aux.cmi
parser_aux.cmi: primitives.cmi

#.depend
breakpoints.cmo : symbols.cmi primitives.cmi pos.cmi ocabug_misc.cmo \
    ocabug_event_boxes.cmo exec.cmi debugcom.cmi checkpoints.cmi \
    breakpoints.cmi
breakpoints.cmx : symbols.cmx primitives.cmx pos.cmx ocabug_misc.cmx \
    ocabug_event_boxes.cmx exec.cmx debugcom.cmx checkpoints.cmx \
    breakpoints.cmi
breakpoints.cmi : primitives.cmi
checkpoints.cmo : primitives.cmi int64ops.cmi debugcom.cmi checkpoints.cmi
checkpoints.cmx : primitives.cmx int64ops.cmx debugcom.cmx checkpoints.cmi
checkpoints.cmi : primitives.cmi debugcom.cmi
command_line.cmo : unix_tools.cmi time_travel.cmi symbols.cmi source.cmi \
    show_source.cmi show_information.cmi question.cmi program_management.cmi \
    program_loading.cmi printval.cmi primitives.cmi pos.cmi parser_aux.cmi parser_debug.cmi \
    parameters.cmi ocabug_view.cmo ocabug_misc.cmo ocabug_config.cmo \
    loadprinter.cmi lexer_debug.cmi int64ops.cmi input_handling.cmi history.cmi \
    frames.cmi events.cmi eval.cmi envaux.cmi debugger_config.cmi \
    debugcom.cmi checkpoints.cmi breakpoints.cmi command_line.cmi
command_line.cmx : unix_tools.cmx time_travel.cmx symbols.cmx source.cmx \
    show_source.cmx show_information.cmx question.cmx program_management.cmx \
    program_loading.cmx printval.cmx primitives.cmx pos.cmx parser_aux.cmi \
    parameters.cmx ocabug_view.cmx ocabug_misc.cmx ocabug_config.cmx \
    loadprinter.cmx lexer_debug.cmi int64ops.cmx input_handling.cmx history.cmx \
    frames.cmx events.cmx eval.cmx envaux.cmx debugger_config.cmx \
    debugcom.cmx checkpoints.cmx breakpoints.cmx command_line.cmi
command_line.cmi :
debugcom.cmo : primitives.cmi int64ops.cmi input_handling.cmi debugcom.cmi
debugcom.cmx : primitives.cmx int64ops.cmx input_handling.cmx debugcom.cmi
debugcom.cmi : primitives.cmi
debugger_config.cmo : int64ops.cmi debugger_config.cmi
debugger_config.cmx : int64ops.cmx debugger_config.cmi
debugger_config.cmi :
envaux.cmo : envaux.cmi
envaux.cmx : envaux.cmi
envaux.cmi :
eval.cmo : printval.cmi parser_aux.cmi frames.cmi debugcom.cmi eval.cmi
eval.cmx : printval.cmx parser_aux.cmi frames.cmx debugcom.cmx eval.cmi
eval.cmi : parser_aux.cmi debugcom.cmi
events.cmo : ocabug_view.cmo ocabug_event_boxes.cmo events.cmi
events.cmx : ocabug_view.cmx ocabug_event_boxes.cmx events.cmi
events.cmi :
exec.cmo : exec.cmi
exec.cmx : exec.cmi
exec.cmi :
frames.cmo : symbols.cmi events.cmi debugcom.cmi frames.cmi
frames.cmx : symbols.cmx events.cmx debugcom.cmx frames.cmi
frames.cmi : primitives.cmi
history.cmo : primitives.cmi int64ops.cmi debugger_config.cmi \
    checkpoints.cmi history.cmi
history.cmx : primitives.cmx int64ops.cmx debugger_config.cmx \
    checkpoints.cmx history.cmi
history.cmi :
input_handling.cmo : primitives.cmi ocabug_misc.cmo input_handling.cmi
input_handling.cmx : primitives.cmx ocabug_misc.cmx input_handling.cmi
input_handling.cmi : primitives.cmi
int64ops.cmo : int64ops.cmi
int64ops.cmx : int64ops.cmi
int64ops.cmi :
lexer_debug.cmi :
loadprinter.cmo : printval.cmi loadprinter.cmi
loadprinter.cmx : printval.cmx loadprinter.cmi
loadprinter.cmi :
main_debug.cmo : unix_tools.cmi time_travel.cmi show_information.cmi question.cmi \
    program_management.cmi primitives.cmi parameters.cmi ocabug_misc.cmo \
    ocabug_controller.cmo ocabug_config.cmo input_handling.cmi frames.cmi \
    exec.cmi debugger_config.cmi command_line.cmi checkpoints.cmi
main_debug.cmx : unix_tools.cmx time_travel.cmx show_information.cmx question.cmx \
    program_management.cmx primitives.cmx parameters.cmx ocabug_misc.cmx \
    ocabug_controller.cmx ocabug_config.cmx input_handling.cmx frames.cmx \
    exec.cmx debugger_config.cmx command_line.cmx checkpoints.cmx
ocabug_config.cmo :
ocabug_config.cmx :
ocabug_controller.cmo : symbols.cmi source.cmi program_management.cmi \
    parameters.cmi ocabug_view.cmo ocabug_misc.cmo ocabug_event_boxes.cmo \
    ocabug_config.cmo events.cmi debugger_config.cmi command_line.cmi \
    breakpoints.cmi
ocabug_controller.cmx : symbols.cmx source.cmx program_management.cmx \
    parameters.cmx ocabug_view.cmx ocabug_misc.cmx ocabug_event_boxes.cmx \
    ocabug_config.cmx events.cmx debugger_config.cmx command_line.cmx \
    breakpoints.cmx
ocabug_event_boxes.cmo : ocabug_view.cmo ocabug_misc.cmo
ocabug_event_boxes.cmx : ocabug_view.cmx ocabug_misc.cmx
ocabug_misc.cmo : ocabug_view.cmo ocabug_config.cmo
ocabug_misc.cmx : ocabug_view.cmx ocabug_config.cmx
ocabug_view.cmo : ocabug_icons.cmo 
parameters.cmo : primitives.cmi envaux.cmi debugger_config.cmi \
    parameters.cmi
parameters.cmx : primitives.cmx envaux.cmx debugger_config.cmx \
    parameters.cmi
parameters.cmi :
parser_aux.cmi : primitives.cmi
pattern_matching.cmo : parser_aux.cmi debugger_config.cmi debugcom.cmi \
    pattern_matching.cmi
pattern_matching.cmx : parser_aux.cmi debugger_config.cmx debugcom.cmx \
    pattern_matching.cmi
pattern_matching.cmi : parser_aux.cmi debugcom.cmi
pos.cmo : source.cmi primitives.cmi pos.cmi
pos.cmx : source.cmx primitives.cmx pos.cmi
pos.cmi :
primitives.cmo : ocabug_config.cmo primitives.cmi
primitives.cmx : ocabug_config.cmx primitives.cmi
primitives.cmi :
printval.cmo : parser_aux.cmi debugcom.cmi printval.cmi
printval.cmx : parser_aux.cmi debugcom.cmx printval.cmi
printval.cmi : parser_aux.cmi debugcom.cmi
program_loading.cmo : unix_tools.cmi primitives.cmi parameters.cmi \
    input_handling.cmi debugger_config.cmi program_loading.cmi
program_loading.cmx : unix_tools.cmx primitives.cmx parameters.cmx \
    input_handling.cmx debugger_config.cmx program_loading.cmi
program_loading.cmi : primitives.cmi
program_management.cmo : unix_tools.cmi time_travel.cmi symbols.cmi \
    question.cmi program_loading.cmi primitives.cmi parameters.cmi \
    ocabug_misc.cmo int64ops.cmi input_handling.cmi history.cmi \
    debugger_config.cmi breakpoints.cmi program_management.cmi
program_management.cmx : unix_tools.cmx time_travel.cmx symbols.cmx \
    question.cmx program_loading.cmx primitives.cmx parameters.cmx \
    ocabug_misc.cmx int64ops.cmx input_handling.cmx history.cmx \
    debugger_config.cmx breakpoints.cmx program_management.cmi
program_management.cmi :
question.cmo : primitives.cmi ocabug_misc.cmo lexer_debug.cmi input_handling.cmi \
    question.cmi
question.cmx : primitives.cmx ocabug_misc.cmx lexer_debug.cmi input_handling.cmx \
    question.cmi
question.cmi :
show_information.cmo : symbols.cmi source.cmi show_source.cmi printval.cmi \
    frames.cmi events.cmi debugcom.cmi checkpoints.cmi breakpoints.cmi \
    show_information.cmi
show_information.cmx : symbols.cmx source.cmx show_source.cmx printval.cmx \
    frames.cmx events.cmx debugcom.cmx checkpoints.cmx breakpoints.cmx \
    show_information.cmi
show_information.cmi :
show_source.cmo : source.cmi primitives.cmi parameters.cmi ocabug_misc.cmo \
    events.cmi debugger_config.cmi show_source.cmi
show_source.cmx : source.cmx primitives.cmx parameters.cmx ocabug_misc.cmx \
    events.cmx debugger_config.cmx show_source.cmi
show_source.cmi :
source.cmo : primitives.cmi debugger_config.cmi source.cmi
source.cmx : primitives.cmx debugger_config.cmx source.cmi
source.cmi :
symbols.cmo : program_loading.cmi events.cmi debugger_config.cmi \
    debugcom.cmi checkpoints.cmi symbols.cmi
symbols.cmx : program_loading.cmx events.cmx debugger_config.cmx \
    debugcom.cmx checkpoints.cmx symbols.cmi
symbols.cmi :
time_travel.cmo : trap_barrier.cmi symbols.cmi question.cmi \
    program_loading.cmi primitives.cmi ocabug_misc.cmo int64ops.cmi \
    input_handling.cmi exec.cmi events.cmi debugger_config.cmi debugcom.cmi \
    checkpoints.cmi breakpoints.cmi time_travel.cmi
time_travel.cmx : trap_barrier.cmx symbols.cmx question.cmx \
    program_loading.cmx primitives.cmx ocabug_misc.cmx int64ops.cmx \
    input_handling.cmx exec.cmx events.cmx debugger_config.cmx debugcom.cmx \
    checkpoints.cmx breakpoints.cmx time_travel.cmi
time_travel.cmi : primitives.cmi
trap_barrier.cmo : exec.cmi debugcom.cmi checkpoints.cmi trap_barrier.cmi
trap_barrier.cmx : exec.cmx debugcom.cmx checkpoints.cmx trap_barrier.cmi
trap_barrier.cmi :
ui_starter.cmo : ocabug_icons.cmo symbols.cmi program_management.cmi parameters.cmi \
    ocabug_misc.cmo ocabug_config.cmo debugger_config.cmi command_line.cmi
ui_starter.cmx : symbols.cmx program_management.cmx parameters.cmx \
    ocabug_misc.cmx ocabug_config.cmx debugger_config.cmx command_line.cmx
unix_tools.cmo : primitives.cmi unix_tools.cmi
unix_tools.cmx : primitives.cmx unix_tools.cmi
unix_tools.cmi :
