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
        'Examples.Automata.DFA.Turnstile': {
            src: 'examples/Examples/Automata/DFA/Turnstile.purs',
            index: 'examples/Examples/Automata/DFA/Turnstile/index.js'
        },
        'Examples.Automata.DFA.OddOnes': {
            src: 'examples/Examples/Automata/DFA/OddOnes.purs',
            index: 'examples/Examples/Automata/DFA/OddOnes/index.js'
        }
    },
    manifests: [
        'bower.json',
        'package.json'
    ],
    output: 'output'
};

var options = {
    compiler: {},
    examples: {
        'Examples.Automata.DFA.Turnstile': {
            main: 'Examples.Automata.DFA.Turnstile'
        },
        'Examples.Automata.DFA.OddOnes': {
            main: 'Examples.Automata.DFA.OddOnes'
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

gulp.task('examples-Examples.Automata.DFA.OddOnes-compile', ['make'], function() {
    var src = [paths.src, paths.bowerSrc, paths.examples['Examples.Automata.DFA.OddOnes'].src];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.DFA.OddOnes']))
});

gulp.task('examples-Examples.Automata.DFA.OddOnes', ['examples-Examples.Automata.DFA.OddOnes-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.DFA.OddOnes'].index)
        .pipe(run('node'));
});

gulp.task('examples-Examples.Automata.DFA.Turnstile-compile', ['make'], function() {
    var src = [paths.src, paths.bowerSrc, paths.examples['Examples.Automata.DFA.Turnstile'].src];
    return gulp.src(src)
        .pipe(purescript.pscMake(options.examples['Examples.Automata.DFA.Turnstile']))
});

gulp.task('examples-Examples.Automata.DFA.Turnstile', ['examples-Examples.Automata.DFA.Turnstile-compile'], function() {
    process.env.NODE_PATH = path.resolve(paths.output);
    return gulp.src(paths.examples['Examples.Automata.DFA.Turnstile'].index)
        .pipe(run('node'));
});

gulp.task('examples', ['examples-Examples.Automata.DFA.Turnstile', 'examples-Examples.Automata.DFA.OddOnes']);

gulp.task('watch-browser', function() {
    gulp.watch(paths.src, ['browser', 'docs']);
});

gulp.task('watch-make', function() {
    gulp.watch(paths.src, ['make', 'docs']);
});

gulp.task('default', ['make', 'docs']);
