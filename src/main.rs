// mod lib;

// use asca;

fn main() {

    // seg::test_node_variants();

    let unparsed_rules: Vec<String> = vec![
        // Proto-Anaki to Anaki
        // Low Vowel Reduction        
        String::from("a > ɐ"),
        String::from("ɐ:[-str] > ə | ɐ{h, ʔ}_"),
        // Glottal Deletion
        String::from("h, ʔ > *"),
        // Clustering I
        String::from("ə$ > * / s_[+cons, -son, -cont, -voice]"),
        // Sonorant Syllabication
        String::from("[+son, -syll]=1 > 1:[+syll] / Cə_əC , Cə_ə#, #ə_əC"),
        String::from("ə > * / _,[+cons, +syll]"),
        // Clustering II
        String::from("ə > * / s_[+cons, +son]"),
        String::from("s$ > & / _[+cons, +son]"),
        // Clustering IV
        String::from("ə > * / [+cons, -syll]_[+son, +cont]V"),
        String::from("[+cons, -syll]$ > & / _[+son, +cont]"),
        String::from("ə > * / VC:[+son, +cont]_C"),
        String::from("$C:[+son, +cont] > & / _$"),
        // 
        String::from("ə > * / VC_s"),
        String::from("$ > * / V_Cs"),
        // Clustering III
        String::from("ə > * / VC:[+nas]_C:[-nas]"),
        String::from("$C:[+nas] > & / V_$C:[-nas]"),
        // Clustering V
        // String::from("ə$ > * / C:[-cont, -nas, αPLACE]_C:[+nasal, -αPLACE]"),
        String::from("ə$ > * / P:[-nas, αPLACE]_N:[-αPLACE]"),
        // Schwa Hiatus Lengthening
        String::from("V:[-str] > [+long] / _,ə"),
        String::from("ə > * / _,V:[-str]"),
        // String::from("ə > 1 / _,V:[-str]=1"),
        // String::from("V:[-str]=1ə > 1:[+long] / _"),
        // Vowel Hiatus Merger
        String::from("$ > * / V:[-long]=1_1"),
        // Schwa Fronting
        String::from("{ɐ, ə} > e / _,i"),
        // Height Assimilation I
        String::from("{ɐ, ə} > [-low, +tense, -red, αhigh, -βback, -γfront, -δround] / _,V:[-low, αhigh, βback, γfront, δround]"),
        // Height Assimilation II
        String::from("V:[-low, +front] > [αhigh] / _,V:[-low, +back, αhigh]"),
        // Catalan-ish Vowel Reduction
        String::from("V:[-lo, -str, -long, -red] > [+hi] | C:[-fr, +bk, -hi, -lo]_"),
        // 2nd Vowel Hiatus Merger
        String::from("$ > * / V:[-long]=1_1"),
        // Uvular Lowering
        String::from("{ɐ, ə}, i, u > ɑ, e, o / [+cons, -high, +back]_"),
        // Loss of Schwa
        String::from("ə > * / _#"),
        String::from("$C > & / _#"),
        // Dorsal Nasal Merger
        String::from("ɴ, ɴʷ > ŋ, ŋʷ / _"),
        // Intervocalic Gliding
        String::from("V:[+hi, +tens] > [-syll, -tens] / V_V"),
        String::from("$ > * / V$G_V"),
        // A-Lowering
        String::from("ɐ:[+stress, Along], ə > [-tens], ɐ"),
        // Geminate Avoidance
        String::from("V=1 C:[+long]=2 > 1:[+long]2:[-long]"),
        // // OR
        // String::from("V:[-long] > [+long] / _C:[+long]"), 
        // String::from("C:[+long] > [-long]"), 
        // Cluster Simplification
        // String::from("V=1 C=2 > 1:[+long] / _s2"),
        String::from("V > [+long] / _C=1s1"),
        String::from("C=1 > * / _s1"),
        // Hap(lo)logy
        String::from("$ > * / C=1 V:[-str]=2 _ 1 2"), 
        String::from("C=1 > * / 1V:[-str]=2_2"),
        // Labialisation
        String::from("C:[+hi, +bk] > [+rnd] / _w"),
        String::from("w > * / C:[+hi, +bk, +rnd]_"),

        // String::from("C:[+cor] > [Aplace, -lab] / _w:[Aplace]"),

        // String::from("% > 1:[-str] / %:[+str]=1_"),
        // String::from("% => [+str] / _%:[-str]%#"),

        // String::from("* > 1 / #_O:[+nas]V=1"),
        // String::from("* > %:[Astr] / #V_O:[+nas, Astr]"),
        // String::from("%:[+str] > [-str] / _%:[+str]"),

        // String::from("* > 1:[-str]%:[Astr] / #_O:[+nas]V:[Astr]=1")

    ];
        
    let unparsed_words: Vec<String> = vec![
            
        // Proto Anaki Words
        String::from("'ra.ka.sa"),
        String::from("'gʷe.la.sa"),
        String::from("'su.ma.qo"),

        String::from("sa'mu.ha.la"),
        String::from("sa'mi.ha.la"),
        String::from("sa'mo.ha.la"),
        String::from("sa'me.ha.la"),
        String::from("sa'ma.ha.la"),

        String::from("'me.hu"),
        String::from("ka're.hu"),
        String::from("'re.ka.re.hu"),

        String::from("'ku.ŋe"),

        String::from("qo'?e.ta"),
        String::from("pa'mo"),
        String::from("pa'no"),
        String::from("pa'ŋo"),
        String::from("pa'ɴo"),

        String::from("'da.ra.sa.ri"),
        String::from("'dars.ri"),

        String::from("'se.re.re"),
        String::from("'ba.ka.wi"),

        String::from("'se.ra"),
        String::from("'se.se.ra"),
        String::from("'se.ra.e.he.ma"),
        String::from("'se.ra.ka.ra"),
        String::from("'se.se.ra.ka.ra"),
        String::from("ˈse.ra.ɢo.ta"),
        String::from("se.ra'te.?e"),

        String::from("sa'qa.la"),
        String::from("sa.ma'pi"),
        String::from("so'?a.ma"),
        String::from("'?a.so.?a.ma"),

        String::from("sa'we.na"),
        String::from("sa.we.na'te.?e"),
        String::from("sa.we.na'lo.?a"),
        String::from("sa.we.na'lo.?o.sa"),
        String::from("sa.we.na'lo.?o.na"),
        String::from("sa.we.na'lo.?o.ta"),
        String::from("sa.we.na'lo.?a.hi.sa"),
        String::from("sa.we.na'lo.?e.hi.sa"),
        String::from("sa.we.na'lo.?u.hi.sa"),
        String::from("sa.we.na'lo.?o.hi.sa"),

        String::from("'la.hi.sa"),
        String::from("'la.hi.hu"),
        String::from("'ra.ke.sa.sa"),
        String::from("'ra.ka.sa.sa"),
    ];

    // let trace = false;

    let res = asca::run(&unparsed_rules, &unparsed_words);

    for r in res {
        println!("{r}");
    }


    // seg::test_vowel_variants();
}