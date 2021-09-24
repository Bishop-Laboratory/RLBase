window.dash_clientside = Object.assign({}, window.dash_clientside, {
    clientside: {
        load_igv: load_igv,
        load_genome: load_genome,
        load_track: load_track
    }
});

function load_igv() {
    var igvDiv = document.getElementById("igv-div");
    var options =
        {
            genome: "hg19",
            // locus: "chr8:127,736,588-127,739,371",
            // tracks: [
            //     {
            //         "name": "HG00103",
            //         "url": "https://s3.amazonaws.com/1000genomes/data/HG00103/alignment/HG00103.alt_bwamem_GRCh38DH.20150718.GBR.low_coverage.cram",
            //         "indexURL": "https://s3.amazonaws.com/1000genomes/data/HG00103/alignment/HG00103.alt_bwamem_GRCh38DH.20150718.GBR.low_coverage.cram.crai",
            //         "format": "cram"
            //     }
            // ]
        };

    igv.createBrowser(igvDiv, options)
        .then(function (browser) {
            igv.browser = browser;
            console.log("Created IGV browser");
        })
};

function load_genome(clicks, gn_name, gn_fastaurl) {
    console.log(clicks, gn_name, gn_fastaurl);
    if (clicks == undefined) return;
    igv.browser.loadGenome({
        "name": gn_name,
        "fastaURL": gn_fastaurl
    });
};

function load_track(clicks, tr_name, tr_url) {
    console.log(clicks, tr_name, tr_url);
    if (clicks == undefined) return;
    igv.browser.loadTrack({
        url: tr_url,
        label: tr_name
    });
};
