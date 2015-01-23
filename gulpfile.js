'use strict'

var gulp        = require('gulp')
  , bump        = require('gulp-bump')
  , filter      = require('gulp-filter')
  , git         = require('gulp-git')
  , path        = require('path')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , tagVersion  = require('gulp-tag-version')
  ;

var paths = {
    src: 'src/**/*.purs',
    bowerSrc: 'bower_components/purescript-*/src/**/*.purs',
    dest: '',
    docsDest: 'docs/README.md',
    examples: {
        'Examples.Automata.Regular.DFA.OddOnes': {
            index: 'examples/Examples/Automata/Regular/DFA/OddOnes/index.js'
        },
        'Examples.Automata.Regular.DFA.Turnstile': {
            index: 'examples/Examples/Automata/Regular/DFA/Turnstile/index.js'
        },
        'Examples.Automata.Regular.DFA.ZeroZeroOne': {
            index: 'examples/Examples/Automata/Regular/DFA/ZeroZeroOne/index.js'
        },
        'Examples.Automata.Regular.NFA.A': {
            index: 'examples/Examples/Automata/Regular/NFA/A/index.js'
        },
        'Examples.Automata.Regular.NFA.B': {
            index: 'examples/Examples/Automata/Regular/NFA/B/index.js'
        },
        'Examples.Automata.Regular.NFA.AB': {
            index: 'examples/Examples/Automata/Regular/NFA/AB/index.js'
        },
        'Examples.Automata.Regular.NFA.OneThirdFromEnd': {
            index: 'examples/Examples/Automata/Regular/NFA/OneThirdFromEnd/index.js'
        }
    },
    examplesSrc: 'examples/**/*.purs',
    manifests: [
        'bower.json',
        'package.json'
    ],
    output: 'output'
};

var options = {
    compiler: {},
    examples: {
        'Examples.Automata.Regular.DFA.OddOnes': {
            main: 'Examples.Automata.Regular.DFA.OddOnes'
        },
        'Examples.Automata.Regular.DFA.Turnstile': {
            main: 'Examples.Automata.Regular.DFA.Turnstile'
        },
        'Examples.Automata.Regular.DFA.ZeroZeroOne': {
            main: 'Examples.Automata.Regular.DFA.ZeroZeroOne'
        },
        'Examples.Automata.Regular.NFA.A': {
            main: 'Examples.Automata.Regular.NFA.A'
        },
        'Examples.Automata.Regular.NFA.B': {
            main: 'Examples.Automata.Regular.NFA.B'
        },
        'Examples.Automata.Regular.NFA.AB': {
            main: 'Examples.Automata.Regular.NFA.AB'
        },
        'Examples.Automata.Regular.NFA.OneThirdFromEnd': {
            main: 'Examples.Automata.Regular.NFA.OneThirdFromEnd'
        }
    },
    pscDocs: {}
};

var compile = function(compiler) {
    var psc = compiler(options.compiler);
    psc.on('error', function(e) {
        console.error(e.message);
        psc.end();
    });
    return gulp.src([paths.src, paths.bowerSrc])
        .pipe(psc)
        .pipe(gulp.dest(paths.dest));
};

function bumpType(type) {
    return gulp.src(paths.manifests)
        .pipe(bump({type: type}))
        .pipe(gulp.dest('./'));
}

gulp.task('tag', function() {
    return gulp.src(paths.manifests)
        .pipe(git.commit('Update versions.'))
        .pipe(filter('bower.json'))
        .pipe(tagVersion());
});

gulp.task('bump-major', function() {
    return bumpType('major')
});
gulp.task('bump-minor', function() {
    return bumpType('minor')
});
gulp.task('bump-patch', function() {
    return bumpType('patch')
});

gulp.task('bump-tag-major', function() {
    return runSequence('bump-major', 'tag');
});
gulp.task('bump-tag-minor', function() {
    return runSequence('bump-minor', 'tag');
});
gulp.task('bump-tag-patch', function() {
    return runSequence('bump-patch', 'tag');
});

gulp.task('make', function() {
    return compile(purescript.pscMake);
});

gulp.task('browser', function() {
    return compile(purescript.psc);
});

gulp.task('docs', function() {
    var pscDocs = purescript.pscDocs(options.pscDocs);
    pscDocs.on('error', function(e) {
        console.error(e.message);
        pscDocs.end();
    });
    return gulp.src(paths.src)
      .pipe(pscDocs)
      .pipe(gulp.dest(paths.docsDest));
});

gulp.task('examples-Examples.Automata.Regular.DFA.OddOnes-compile', function() {
    var src = [paths.src, paths.bowerSrc, paths.examplesSrc];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.Regular.DFA.OddOnes']))
});

gulp.task('examples-Examples.Automata.Regular.DFA.OddOnes', ['examples-Examples.Automata.Regular.DFA.OddOnes-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.Regular.DFA.OddOnes'].index)
        .pipe(run('node'));
});

gulp.task('examples-Examples.Automata.Regular.DFA.Turnstile-compile', function() {
    var src = [paths.src, paths.bowerSrc, paths.examplesSrc];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.Regular.DFA.Turnstile']))
});

gulp.task('examples-Examples.Automata.Regular.DFA.Turnstile', ['examples-Examples.Automata.Regular.DFA.Turnstile-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.Regular.DFA.Turnstile'].index)
        .pipe(run('node'));
});

gulp.task('examples-Examples.Automata.Regular.DFA.ZeroZeroOne-compile', function() {
    var src = [paths.src, paths.bowerSrc, paths.examplesSrc];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.Regular.DFA.ZeroZeroOne']))
});

gulp.task('examples-Examples.Automata.Regular.DFA.ZeroZeroOne', ['examples-Examples.Automata.Regular.DFA.ZeroZeroOne-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.Regular.DFA.ZeroZeroOne'].index)
        .pipe(run('node'));
});

gulp.task('examples-Examples.Automata.Regular.NFA.A-compile', function() {
    var src = [paths.src, paths.bowerSrc, paths.examplesSrc];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.Regular.NFA.A']))
});

gulp.task('examples-Examples.Automata.Regular.NFA.A', ['examples-Examples.Automata.Regular.NFA.A-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.Regular.NFA.A'].index)
        .pipe(run('node'));
});

gulp.task('examples-Examples.Automata.Regular.NFA.B-compile', function() {
    var src = [paths.src, paths.bowerSrc, paths.examplesSrc];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.Regular.NFA.B']))
});

gulp.task('examples-Examples.Automata.Regular.NFA.B', ['examples-Examples.Automata.Regular.NFA.B-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.Regular.NFA.B'].index)
        .pipe(run('node'));
});

gulp.task('examples-Examples.Automata.Regular.NFA.AB-compile', function() {
    var src = [paths.src, paths.bowerSrc, paths.examplesSrc];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.Regular.NFA.AB']))
});

gulp.task('examples-Examples.Automata.Regular.NFA.AB', ['examples-Examples.Automata.Regular.NFA.AB-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.Regular.NFA.AB'].index)
        .pipe(run('node'));
});

gulp.task('examples-Examples.Automata.Regular.NFA.OneThirdFromEnd-compile', function() {
    var src = [paths.src, paths.bowerSrc, paths.examplesSrc];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.Regular.NFA.OneThirdFromEnd']))
});

gulp.task('examples-Examples.Automata.Regular.NFA.OneThirdFromEnd', ['examples-Examples.Automata.Regular.NFA.OneThirdFromEnd-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.Regular.NFA.OneThirdFromEnd'].index)
        .pipe(run('node'));
});

gulp.task('examples', function() {
    return runSequence( 'make'
                      , 'examples-Examples.Automata.Regular.DFA.OddOnes'
                      , 'examples-Examples.Automata.Regular.DFA.Turnstile'
                      , 'examples-Examples.Automata.Regular.DFA.ZeroZeroOne'
                      , 'examples-Examples.Automata.Regular.NFA.A'
                      , 'examples-Examples.Automata.Regular.NFA.B'
                      , 'examples-Examples.Automata.Regular.NFA.AB'
                      , 'examples-Examples.Automata.Regular.NFA.OneThirdFromEnd'
                      );
});

gulp.task('watch-browser', function() {
    gulp.watch(paths.src, ['browser', 'docs']);
});

gulp.task('watch-make', function() {
    gulp.watch(paths.src, ['make', 'docs']);
});

gulp.task('default', ['make', 'docs']);
