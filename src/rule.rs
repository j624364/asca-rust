use std::{
    cell::RefCell,
    collections::HashMap, 
    cmp ::max, 
    fmt, 
};

use crate   :: {
    error   :: { Error, RuleSyntaxError }, 
    parser  :: { Item, ParseElement }, 
    seg     :: NodeKind, 
    subrule :: SubRule, 
    word    :: Word, 
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RuleType {
    Substitution,
    Metathesis,
    Deletion,
    Insertion,
}

#[derive(Debug)]
pub struct PlaceMod {
    pub lab: Option<u8>,
    pub cor: Option<u8>,
    pub dor: Option<u8>,
    pub phr: Option<u8>,
}

impl PlaceMod {
    pub fn new(lab: Option<u8>, cor: Option<u8>, dor: Option<u8>, phr: Option<u8>) -> Self {
        Self { lab, cor, dor, phr }
    }
}

#[derive(Debug)]
pub enum Alpha {
    Node(NodeKind, Option<u8>),
    Place(PlaceMod),
    Feature(bool),
    Supra(bool),
}

impl Alpha {
    // /// Returns `true` if the alpha is `Feature`.
    // pub fn is_feature(&self) -> bool {
    //     matches!(self, Self::Feature(..))
    // }

    pub fn as_node(&self) -> Option<(NodeKind, Option<u8>)> {
        if let Self::Node(n, m) = self {
            Some((*n, *m))
        } else {
            None
        }
    }

    // pub fn as_feature(&self) -> Option<&bool> {
    //     if let Self::Feature(pos) = self {
    //         Some(pos)
    //     } else {
    //         None
    //     }
    // }

    pub fn as_place(&self) -> Option<&PlaceMod> {
        if let Self::Place(place) = self {
            Some(place)
        } else {
            None
        }
    }

    // pub fn as_supra(&self) -> Option<(&SupraType, &bool)> {
    //     if let Self::Supra(st, pos) = self {
    //         Some((st, pos))
    //     } else {
    //         None
    //     }
    // }

    pub fn as_binary(&self) -> bool {
        match self {
            Alpha::Feature(pos) | Alpha::Supra(pos) => *pos,
            Alpha::Node(_, node_mod) => node_mod.is_some(),
            Alpha::Place(pm) => pm.lab.is_some() || pm.cor.is_some() || pm.dor.is_some() || pm.phr.is_some(),
        }
    }
}

pub struct Rule {
    pub input:     Vec<Vec<Item>>,    // to support multirules
    pub output:    Vec<Vec<Item>>,    // these need to be Vec<Vec<Item>>
    pub context:   Vec<Item>,
    pub except:    Vec<Item>,
}

impl Rule {
    pub fn new(i: Vec<Vec<Item>>, o: Vec<Vec<Item>>, c :Vec<Item>, e :Vec<Item>) -> Self {
        Self { input: i, output: o, context: c, except: e }
    }

    pub fn split_into_subrules(&self) -> Result<Vec<SubRule>, RuleSyntaxError> {
        // check that input, output, context, except are the same length
        // and if any are not, that they are length == 1
        // context and except can be length == 0
        let max = max(self.input.len(), max(self.output.len(), max(self.context.len(), self.except.len())));

        if self.input.len()   != max && self.input.len()   != 1 { return Err(RuleSyntaxError::UnbalancedRuleIO(self.input.clone()))  }
        if self.output.len()  != max && self.output.len()  != 1 { return Err(RuleSyntaxError::UnbalancedRuleIO(self.output.clone())) }
        if self.context.len() != max && self.context.len() != 1 && !self.context.is_empty() { return Err(RuleSyntaxError::UnbalancedRuleEnv(self.context.clone())) }
        if self.except.len()  != max && self.except.len()  != 1 && !self.except.is_empty()  { return Err(RuleSyntaxError::UnbalancedRuleEnv(self.except.clone()))  }

        // populate subrules, if one's length == 1 then it's value is duplicated to rest of subrules
        let mut sub_vec = Vec::new();
        for i in 0..max {
            let input   = if  self.input.len() == 1 {  self.input[0].clone() } else {  self.input[i].clone() };
            let output  = if self.output.len() == 1 { self.output[0].clone() } else { self.output[i].clone() };
            let context = if self.context.is_empty() { None } else if self.context.len() == 1 { Some(self.context[0].clone()) } else { Some(self.context[i].clone()) };
            let except  = if  self.except.is_empty() { None } else if  self.except.len() == 1 { Some( self.except[0].clone()) } else { Some( self.except[i].clone()) };
            let rule_type = {
                match (&input[0].kind, &output[0].kind) {
                    (ParseElement::EmptySet, ParseElement::EmptySet) => return Err(RuleSyntaxError::InsertDelete(input[0].position.line, input[0].position.start, output[0].position.start)),
                    (ParseElement::EmptySet, ParseElement::Metathesis) => return Err(RuleSyntaxError::InsertMetath(input[0].position.line, input[0].position.start, output[0].position.start)),
                    (ParseElement::EmptySet, _) => RuleType::Insertion,
                    (_, ParseElement::EmptySet) => RuleType::Deletion,
                    (_, ParseElement::Metathesis) => RuleType::Metathesis,
                    (..) => RuleType::Substitution  
                }
            };

            sub_vec.push(
                SubRule {
                    input, 
                    output, 
                    context, 
                    except, 
                    rule_type, 
                    variables: RefCell::new(HashMap::new()), 
                    alphas: RefCell::new(HashMap::new()), 
                    // pos: SegPos::new(0, 0),
                    // state_index: 0,
                }
            );
        }

        Ok(sub_vec)
    }

    pub fn apply(&self, word: Word /*, trace: bool*/) -> Result<Word, Error> {
        
        let sub_rules = self.split_into_subrules()?;
        
        let mut res_word = word; 
        for i in sub_rules {
            res_word = i.apply(res_word)?;
            // println!("{i:#?} ---> {res_word:#?}");
        }
        Ok(res_word)
    }
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Rule ->")?;
        writeln!(f, "    Input = [")?;
        for i in self.input.iter() {
            writeln!(f, "        {i:?}")?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Output = [")?;
        for o in self.output.iter() {
            writeln!(f, "        {o:?}")?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Context = [")?;
        for c in self.context.iter() {
            writeln!(f, "        {c}")?;
        }
        writeln!(f, "    ]")?;
        writeln!(f, "    Exception = [")?;
        for e in self.except.iter() {
            writeln!(f, "        {e}")?;
        }
        writeln!(f, "    ]")?;

        Ok(())
    }
}


#[cfg(test)]
mod rule_tests {
    use crate::ASCAError;

    use super::*;
    
    fn setup_rule(test_str: &str) -> Rule {
        use crate::{Lexer, Parser};

        let maybe_lex = Lexer::new(&test_str.chars().collect::<Vec<_>>(),0).get_line();
        match maybe_lex {
            Ok(lexed) => {
                match Parser::new(lexed, 0).parse() {
                    Ok(rule) => return rule,
                    Err(e) => {
                        println!("{}", e.format_error(&vec![test_str.to_owned()]));
                        assert!(false);
                    },
                }
            },
            Err(e) => {
                println!("{}", e.format_error(&vec![test_str.to_owned()]));
                assert!(false);
            },
        } 
        unreachable!()
    }

    fn setup_word(test_str: &str) -> Word {
        Word::new(String::from(test_str)).unwrap()
    }

    #[test]
    fn test_spec_env() {
        let test_rule = setup_rule("V > * / _,#");
        let test_word = setup_word("a.ri.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ri.s");

        let test_rule = setup_rule("C > * / _,#a");
        let test_word = setup_word("a.ri.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "a.i.a");
    }

    #[test]
    fn test_sub_simple_ipa() {
        let test_rule = setup_rule("r > l");
        let test_word = setup_word("la.ri.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "la.li.sa");
    }

    #[test]
    fn test_sub_insert_length() {
        let test_rule = setup_rule("Vr > [+long]l");
        let test_word = setup_word("dark");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "daːlk");
    }

    #[test]
    fn  test_sub_insert_ipa() {
        let test_rule = setup_rule("a > aeoi");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "daeoik");
    }

    #[test]
    fn  test_sub_insert_syll_bound() {
        let test_rule = setup_rule("a > a$e");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "da.ek");

        let test_rule = setup_rule("k > k$");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "dak");

        let test_rule = setup_rule("k > k$a");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "daka");      // TODO: Should this "dak.a" ?
    }

    #[test]
    fn test_sub_insert_syll() {
        let test_rule = setup_rule("a > a%:[tone:12]e");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "da.ke12");
        let test_word = setup_word("dak.mo");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "da.k12.emo");  // weird, but makes sense
        let test_rule = setup_rule("a > a%:[tone:12]=1e1");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "da.ke12.k12"); // also weird

        let test_rule = setup_rule("k > k%");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "dak");

        let test_rule = setup_rule("k > k%e");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "dake");      // TODO: Should this "dak.e" ?

        let test_rule = setup_rule("k > k%:[tone: 12]e");
        let test_word = setup_word("dak");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "dake");

        // let test_rule = setup_rule("k > k%=1e1");
        // let test_word = setup_word("dak");
        // assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "dake");   // TODO: this errs
    }

    #[test]
    fn test_sub_syll_var() {
        let test_rule = setup_rule("% > 1:[-str] / %:[+str]=1_");
        let test_word = setup_word("keˈsa.lo");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "keˈsa.sa");
    }

    #[test]
    fn test_sub_assim() {
        let test_rule = setup_rule("V > [αround] / _C[αround]");
        let test_word = setup_word("le.ro");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "lø.ro");
    }

    #[test]
    fn test_sub_assim_turk_contrived() {
        let test_rule = setup_rule("[+syll, +hi] > [αbk, βfr, γrnd] / [αbk, βfr, γrnd] CC _C #");
        let test_word = setup_word("røstin");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "røstyn");
        let test_word = setup_word("kɨzlik");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kɨzlɨk");
    }

    #[test]
    fn test_sub_del_ipa() {
        let test_rule = setup_rule("sk > ʃ");
        let test_word = setup_word("skip");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ʃip");
    }

    #[test]
    fn test_sub_del_ipa_bordering() {
        let test_rule = setup_rule("sk > ʃ");
        let test_word = setup_word("skskip");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ʃːip");

        let test_word = setup_word("ask.skip");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "aʃ.ʃip");
    }
    #[test]
    fn test_sub_del_length() {
        let test_rule = setup_rule("Vr > [+long]");
        let test_word = setup_word("dark");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "daːk");
    }

    #[test]
    fn test_sub_set() {
        let test_rule = setup_rule("{p, t, k} > {b, d, g}");
        let test_word = setup_word("pa.ta.ka");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ba.da.ɡa");
    }

    #[test]
    fn test_sub_alpha() {
        let test_rule = setup_rule("d > [αvoice] / _[αvoice]");
        let test_word = setup_word("ad.ha");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "at.ha");

        let test_rule = setup_rule("d > [Avoice] / _[Avoice]");
        let test_word = setup_word("ad.ha");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "at.ha");

        let test_rule = setup_rule("k > [αvoice] / [-αvoice]_");
        let test_word = setup_word("əs.kɔl");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "əs.ɡɔl");

        let test_rule = setup_rule("k > [Bvoice] / [-Bvoice]_");
        let test_word = setup_word("əs.kɔl");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "əs.ɡɔl");
    }

    #[test]
    fn test_met_simple_ipa() {
        let test_rule = setup_rule("sk > &");
        let test_word = setup_word("ˈɑːs.ki.ɑn");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ˈɑːk.si.ɑn");

        let test_rule = setup_rule("[+rhotic]V > & / _s");
        let test_word = setup_word("ˈhros");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ˈhors");

        let test_rule = setup_rule("oba > &");
        let test_word = setup_word("ˈko.ba.lo.ba");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ˈka.bo.la.bo");
    }

    #[test]
    fn test_manual_met() {
        let test_rule = setup_rule("[+rho]=1 V=2 > 2 1  / _s");
        let test_word = setup_word("ˈhros");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ˈhors");
    }

    // #[test]
    // fn test_met_long_dist_ipa() {
    //     let test_rule = setup_rule("r...l > &");
    //     let test_word = setup_word("ˈpa.ra.bo.la");
    //     assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ˈpa.la.bo.ra");
    // }

    #[test]
    fn test_met_syll_bound() {
        let test_rule = setup_rule("$s > & / V_{p,t,k}");
        let test_word = setup_word("e.spa.ɲa");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "es.pa.ɲa");
    }

    #[test]
    fn test_met_syll() {
        let test_rule = setup_rule("%% > &");
        let test_word = setup_word("sa.ro.na");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ro.sa.na");
    }

    #[test]
    fn test_met_ident() {
        let test_rule = setup_rule("V > &");
        let test_word = setup_word("saus");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "saus");
    }

    #[test]
    fn test_met_simple_mixed() {
        let test_rule = setup_rule("lVr > &");
        let test_word = setup_word("la.ri");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ra.li");
        // V matches a vowel segment of arbitrary length (user must specify [-long] if only matching short segments)
        let test_word = setup_word("la:.ri");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "raː.li");
        // But does not match different consecutive vowels
        let test_word = setup_word("lau.ri");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "lau.ri");
    }

    #[test]
    fn test_del_simple_ipa() {
        let test_rule = setup_rule("o > *");
        let test_word = setup_word("o.so.on.o");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "s.n");
    }

    #[test]
    fn test_del_syll() {
        let test_rule = setup_rule("% > * | _s");
        let test_word = setup_word("a.ske.sa.re");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "a.ske");
    }

    #[test]
    fn test_del_syll_var() {
        let test_rule = setup_rule("%=1 > * / 1_");
        let test_word = setup_word("ke.sa.sa");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.sa");
    }

    #[test]
    fn test_del_ipa_before_wbound() {
        let test_rule = setup_rule("t > *  / _#");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kat.ka");
    }

    #[test]
    fn test_del_ipa_before_sbound() {
        let test_rule = setup_rule("t > *  / _$ | _#");
        let test_word = setup_word("kat.kat");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ka.kat");
    }
    
    #[test]
    fn test_del_vowel_after_vowel() {
        let test_rule = setup_rule("V > * / V_");
        let test_word = setup_word("kai.lua");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ka.lu");
    }

    #[test]
    fn test_del_matrix_after_matrix() {
        let test_rule = setup_rule("[+syll, +high] > * / [+syll, -high]_");
        let test_word = setup_word("kai.lua");
        // from Assamese, "a high vowel gets deleted following a non-high vowel"
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ka.lua");
    }

    #[test]
    fn test_except_before_simple_ipa() {
        let test_rule = setup_rule(" i > e | c_");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.ci");
    }

    #[test]
    fn test_except_before_ipa() {
        let test_rule = setup_rule(" i > e | c:[+long] _");
        let test_word = setup_word("ki.cːi");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.cːi");
    }

    #[test]
    fn test_except_before_ipa_bound() {
        let test_rule = setup_rule(" i > e | cc_");
        let test_word = setup_word("kic.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kec.ci");
    }

    #[test]
    fn test_except_after_simple_ipa() {
        let test_rule = setup_rule(" i > e | _c");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ki.ce");
    }

    #[test]
    fn test_except_after_ipa() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("ki.cːi");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ki.cːe");
    }

    #[test]
    fn test_except_after_ipa_bound() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("kic.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kic.ce");
    }

    #[test]
    fn test_except_before_ipa_bound_false() {
        let test_rule = setup_rule(" i > e | cc_");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.ce");
    }

    #[test]
    fn test_except_after_ipa_bound_false() {
        let test_rule = setup_rule(" i > e | _cc");
        let test_word = setup_word("ki.ci");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ke.ce");
    }

    #[test]
    fn test_context_set() {
        let test_rule = setup_rule("i > ɛ / _{r,h,ʍ}");
        let test_word = setup_word("si.sir");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "si.sɛr");
        
        let test_word = setup_word("si.si.haz");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "si.sɛ.haz");

        let test_word = setup_word("ri.hi.ʍaz");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "rɛ.hɛ.ʍaz");
    }

    #[test]
    fn test_insertion_context_ipa() {
        let test_rule = setup_rule("* > e / _s");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "eski");

        let test_rule = setup_rule("* > e / s_");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "seki");

        let test_rule = setup_rule("* > e / s_");
        let test_word = setup_word("kas");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "kase");
        
        let test_rule = setup_rule("* > e / _k");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "seki");

        let test_rule = setup_rule("* > e / k_");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "skei");

        // let test_rule = setup_rule("* > e / s_k");
        // let test_word = setup_word("kskis");
        // assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "ksekis");
    }

    #[test]
    fn test_insertion_context_set() {
        let test_rule = setup_rule("* > e / _{s,k}");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "eseki");

        let test_rule = setup_rule("* > e / {s,k}_");
        let test_word = setup_word("ski");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "sekei");
    }

    #[test]
    fn test_insertion_context_matrix() {
        let test_rule = setup_rule("* > e / _C");
        let test_word = setup_word("ski");
        println!("* > e / _C");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "eseki");
    }

    #[test]
    fn test_insertion_spanish() {
        // TODO(girv): insertion cannot match set in context to set in output
        // let test_rule = setup_rule("* > {b, d} / {m, n}_r");
        let test_rule = setup_rule("* > b, d / m_r, n_r");
        let test_word = setup_word("om.re");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "om.bre");
    }

    #[test]
    fn test_insertion_context_syll() {
        let test_rule = setup_rule("* > e / _%");
        let test_word = setup_word("s.ki");
        println!("* > e / _%");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "se.ki");
    }

    #[test]
    fn test_insertion_context_syll_bound() {
        let test_rule = setup_rule("* > e / _$ | _#");
        let test_word = setup_word("s.ki");
        println!("* > e / _$");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "se.ki");

        let test_rule = setup_rule("* > e / $_ | #_");
        let test_word = setup_word("as.k");
        println!("* > e / $_");
        assert_eq!(test_rule.apply(test_word).unwrap().render().unwrap(), "as.ek");
    }


    #[test]
    fn test_match_alpha_feature() {
        let test_rule = setup_rule("V > [αnasal] / _[αnasal]");

        assert_eq!(test_rule.apply(setup_word("an.ti")).unwrap().render().unwrap(), "ãn.ti");
        assert_eq!(test_rule.apply(setup_word("na.ti")).unwrap().render().unwrap(), "na.ti");
        assert_eq!(test_rule.apply(setup_word("tan")).unwrap().render().unwrap(), "tãn");
    }

    #[test]
    fn test_nasal_assim() {
        let test_rule = setup_rule("[+nasal] > [αPLACE] / _C:[αPLACE]");
        assert_eq!(test_rule.apply(setup_word("ˈsɑm.dɑz")).unwrap().render().unwrap(), "ˈsɑn.dɑz");
        assert_eq!(test_rule.apply(setup_word("ˈhʊng")).unwrap().render().unwrap(), "ˈhʊŋɡ");
        assert_eq!(test_rule.apply(setup_word("ˈɪn.pʊt")).unwrap().render().unwrap(), "ˈɪm.pʊt");
    }

    #[test]
    fn test_portuguese() {
        let test_rules = [
            setup_rule("[+rho] > [-cont] / C_, _$"),
            setup_rule("{s,m} > * / _#"),
            setup_rule("k > t^s / _[+front]"),
            setup_rule("i > j / _V"),
            setup_rule("V:[+long] > [-long]"),
            setup_rule("e > * / Vr_#"),
            setup_rule("$ > * / _r#"),
            setup_rule("$w > * / V_V"),
            setup_rule("u > o / _#"),
            setup_rule("ŋn > ɲ"),
            setup_rule("O:[-dr, -voice] > [+voice] / V_V"), // p,t,k > b,d,g / V_V
            setup_rule("k > i / i_t, e_t"), 
            setup_rule("k > u / u_t, o_t"), 
            setup_rule("O:[-cont] > 1 / V_O:[-cont]=1"),    // p > t / V_t
            setup_rule("i:[+long] > [-long]"),
            setup_rule("e > * / C_rV"),
            setup_rule("t^s > s"),
            setup_rule("lj > ʎ"),
            // setup_rule("j > * / ʎ_"),
            setup_rule("s > ʃ / i_"),
            setup_rule("j > ʒ"),
            setup_rule("a:[-str], e:[-str], o:[-str] > ɐ, ɨ, u | _CC"),
            setup_rule("C=1 > * / _1"),
            setup_rule("O:[+voice] > [+cont] | #_"),           // b, d, g > β, ð, ɣ | #_
            setup_rule("C$ > & / $_"),
            setup_rule("$C > & / _$"),
            setup_rule("V:[+str] > [αnasal] / _[αnasal]C"),
            setup_rule("N > * / V:[+nasal]_"),
        ];
        let test_words = [
            setup_word("'fo.kus"),
            setup_word("'jo.kus"),
            setup_word("dis'trik.tus"),
            setup_word("ki:.wi'ta:.tem"),
            setup_word("a.dop'ta.re"),
            setup_word("'o.pe.ra"),
            setup_word("se'kun.dus"),
            setup_word("'fi:.liam"),
            setup_word("'po:n.tem"),
        ];
        let output_matchs = [
            setup_word("ˈfo.ɣu"),
            setup_word("ˈʒo.ɣu"),
            setup_word("diʃˈtɾi.tu"),
            setup_word("siˈða.ðɨ"),
            setup_word("ɐ.ðoˈtar"),
            setup_word("ˈo.βrɐ"),   // ˈɔ.βɾɐ
            setup_word("sɨˈɣũ.ðu"),
            setup_word("ˈfi.ʎɐ"),
            setup_word("ˈpõ.tɨ"),
        ];

        let mut output_words: Vec<Word> = vec![];

        for word in &test_words {
            let mut w = word.clone();
            for rule in &test_rules {
                println!("---");
                println!("{}", w.render().unwrap());
                w = rule.apply(w).unwrap();
            }
            output_words.push(w)
        }

        for (w, m) in output_words.iter().zip(output_matchs) {
            assert_eq!(w.render().unwrap(), m.render().unwrap());
        }
    }

    #[test]
    fn test_proto_germanic_spirant_law() {
        // (dʰt, dt, tt >) ts(t) > ss
        // (dʰs, ds, ts >) ts > ss
        // (bʰs, bs, ps >) ps > ɸs
        // (ɡʰs, ɡs, ks >) ks > xs
        // (bʰt, bt, pt >) pt > ɸt
        // (ɡʰt, ɡt, kt >) kt > xt

        // dʰt, dt, tt > tst
        // tst > ts
        // dʰs, ds > ts
        // {p, t, k} > {ɸ, s, x} / _{t,s}


        let test_rule = setup_rule("{p, t, k} > {ɸ, s, x} / _{t,s}");

        assert_eq!(test_rule.apply(setup_word("ˈɑp.ter")).unwrap().render().unwrap(), "ˈɑɸ.ter");
        assert_eq!(test_rule.apply(setup_word("ˈɑp.sɑn")).unwrap().render().unwrap(), "ˈɑɸ.sɑn");

        assert_eq!(test_rule.apply(setup_word("ˈɑt.ter")).unwrap().render().unwrap(), "ˈɑs.ter");
        assert_eq!(test_rule.apply(setup_word("ˈɑt.sɑn")).unwrap().render().unwrap(), "ˈɑs.sɑn");

        assert_eq!(test_rule.apply(setup_word("ˈɑk.ter")).unwrap().render().unwrap(), "ˈɑx.ter");
        assert_eq!(test_rule.apply(setup_word("ˈɑk.sɑn")).unwrap().render().unwrap(), "ˈɑx.sɑn");

    }

    #[test]
    fn test_grimms_law() {
        let test_rule = setup_rule("[+cons, -son, -voice, -cont], [+cons, -son, +voice, -cont, -sg], [+cons, +voice, +sg] > [+cont], [-voice], [-sg]");
        
        assert_eq!(test_rule.apply(setup_word("kunˈtos")).unwrap().render().unwrap(), "xunˈθos");
        assert_eq!(test_rule.apply(setup_word("ˈdant")).unwrap().render().unwrap(), "ˈtanθ");
        assert_eq!(test_rule.apply(setup_word("ˈme.dʱu")).unwrap().render().unwrap(), "ˈme.du");
        assert_eq!(test_rule.apply(setup_word("ˈkʷod")).unwrap().render().unwrap(), "ˈxʷot");
        assert_eq!(test_rule.apply(setup_word("'ɡʱans")).unwrap().render().unwrap(), "ˈɡans");
        assert_eq!(test_rule.apply(setup_word("'ɡʷʱels")).unwrap().render().unwrap(), "ˈɡʷels");
    }

    #[test]
    fn test_verners_law() {
        // let test_rule = setup_rule("[-voice, +cont] > [+voice] / V:[-stress]([+son])_");
        let test_rule = setup_rule("[-voice, +cont] > [+voice] / V:[-stress]_, V:[-stress][+son]_");
        
        assert_eq!(test_rule.apply(setup_word("xunˈθos")).unwrap().render().unwrap(), "xunˈðos");
        assert_eq!(test_rule.apply(setup_word("fɑˈθer")).unwrap().render().unwrap(), "fɑˈðer");
        assert_eq!(test_rule.apply(setup_word("uˈɸer")).unwrap().render().unwrap(), "uˈβer");
        assert_eq!(test_rule.apply(setup_word("ɑɸ")).unwrap().render().unwrap(), "ɑβ");
        assert_eq!(test_rule.apply(setup_word("ˈme.du")).unwrap().render().unwrap(), "ˈme.du");
    }

    #[test]
    fn test_pgmc_stress_shift() {
        let test_rule = setup_rule("%:[+stress], % > [-stress], [+stress] / _, #_");
        assert_eq!(test_rule.apply(setup_word("xunˈðos")).unwrap().render().unwrap(), "ˈxun.ðos");
        assert_eq!(test_rule.apply(setup_word("fɑˈðer")).unwrap().render().unwrap(), "ˈfɑ.ðer");
        assert_eq!(test_rule.apply(setup_word("uˈβer")).unwrap().render().unwrap(), "ˈu.βer");
    }

    #[test]
    fn test_japanese_devoicing() {
        // let test_rule = setup_rule("V > [-voice] / [-voi]_{[-voi], #}");
        let test_rule = setup_rule("V > [-voice] / [-voi]_[-voi], [-voi]_#");

        assert_eq!(test_rule.apply(setup_word("de.sɯ")).unwrap().render().unwrap(), "de.sɯ̥");
    }

    #[test]
    fn test_latin_stress() {
        let test_rule = setup_rule("% => [+str] / #_#");
        assert_eq!(test_rule.apply(setup_word("sar")).unwrap().render().unwrap(), "ˈsar");
        let test_rule = setup_rule("V:[+lng]=> [+str] / _%#");
        assert_eq!(test_rule.apply(setup_word("peː.diː.kaː.boː")).unwrap().render().unwrap(), "peː.diːˈkaː.boː");
        // let test_rule = setup_rule("V => [+str] / _C%#");
        let test_rule = setup_rule("C => [+str] / _%#");
        assert_eq!(test_rule.apply(setup_word("kae̯.sar")).unwrap().render().unwrap(), "ˈkae̯.sar");
        assert_eq!(test_rule.apply(setup_word("de.kem.ber")).unwrap().render().unwrap(), "deˈkem.ber");
        let test_rule = setup_rule("% => [+str] / _%:[-str]%#");
        assert_eq!(test_rule.apply(setup_word("juː.li.us")).unwrap().render().unwrap(), "ˈjuː.li.us");
        assert_eq!(test_rule.apply(setup_word("a.ba.ki.noː")).unwrap().render().unwrap(), "aˈba.ki.noː"); 

        let test_rule = setup_rule("%, V:[+lng], C, % => [+str] / #_#, _%#, _%#, _%:[-str]%#");
        assert_eq!(test_rule.apply(setup_word("sar")).unwrap().render().unwrap(), "ˈsar");
        assert_eq!(test_rule.apply(setup_word("peː.diː.kaː.boː")).unwrap().render().unwrap(), "peː.diːˈkaː.boː");
        assert_eq!(test_rule.apply(setup_word("kae̯.sar")).unwrap().render().unwrap(), "ˈkae̯.sar");
        assert_eq!(test_rule.apply(setup_word("de.kem.ber")).unwrap().render().unwrap(), "deˈkem.ber");
        assert_eq!(test_rule.apply(setup_word("juː.li.us")).unwrap().render().unwrap(), "ˈjuː.li.us");
        assert_eq!(test_rule.apply(setup_word("a.ba.ki.noː")).unwrap().render().unwrap(), "aˈba.ki.noː"); 
        assert_eq!(test_rule.apply(setup_word("sep.ti.mus")).unwrap().render().unwrap(), "ˈsep.ti.mus"); 
        assert_eq!(test_rule.apply(setup_word("sep.tem.ber")).unwrap().render().unwrap(), "sepˈtem.ber"); 
    }

    #[test]
    fn test_engala_thingy() {
        let test_rule = setup_rule("O:[+nas, Aplace]=1,$N > n:[Aplace]1:[-nas], & / _ , V_C");
        assert_eq!(test_rule.apply(setup_word("a.ᵐbo")).unwrap().render().unwrap(), "am.bo");
    }

    // #[test]
    // fn test_latin_stress() {
    //     let test_rule = setup_rule("");
    //     assert_eq!(test_rule.apply(setup_word("")).unwrap().render().unwrap(), "");
    // }
}