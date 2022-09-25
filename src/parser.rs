extern crate swc_common;
extern crate swc_ecma_parser;
use std::path::Path;

use swc_common::sync::Lrc;
use swc_common::{
    errors::{ColorConfig, Handler},
    SourceMap,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};
use swc_ecma_ast::{Module};

const TARGET_ES_VERSION: swc_ecma_ast::EsVersion = swc_ecma_ast::EsVersion::Es2015;

pub fn parse_file(path: &Path) -> Result<Module, std::io::Error> {
    let cm: Lrc<SourceMap> = Default::default();
    let handler =
        Handler::with_tty_emitter(ColorConfig::Auto, true, false,
        Some(cm.clone()));

    let fm = cm
        .load_file(path)?;

    let lexer = Lexer::new(
        Syntax::Es(Default::default()),
        TARGET_ES_VERSION,
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let module = parser
        .parse_module()
        .map_err(|e| {
            // Unrecoverable fatal error occurred
            e.into_diagnostic(&handler).emit();
            std::io::Error::new(std::io::ErrorKind::Other, "failed to parse module")
        })?;
    
    Ok(module)
}
