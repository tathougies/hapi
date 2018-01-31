var decodeBase64Url = function (x) {
    return atob(x.replace(/\-/g, '+').replace(/_/g, '/'));
};

window.submitHapiForm = function (e) {
    e = e || window.event;

    e.preventDefault();

    var tgt = $(e.target);
    var form = tgt.closest('form');
    var controls = form.find('[data-hapi-path]');

    var o = {};

    controls.each(function () {
        var control = $(this);
        var x = null;
        switch (control.get()[0].tagName) {
        case "DIV":
            if ( control.is("[data-hapi-type=multi]") ) {
                var type = JSON.parse(decodeBase64Url(control.attr("data-hapi-type-name")));
                x = { "$type": type, data: {} };
            }
            break;
        case "INPUT":
            switch ( control.attr('type').toUpperCase() ) {
            case "CHECKBOX":
              x = control.is(":checked");
              break;
            case "TEXT":
            case "PASSWORD":
            case "NUMBER":
            default:
              if (!control.is(":disabled"))
                  x = control.val();
              if (control.is("[data-hapi-type=credential]"))
                  x = {secret: x};
              if (control.is("[data-hapi-type=number]"))
                  x = parseInt(x);
            }
            break;
        case "SELECT":
            x = JSON.parse(decodeBase64Url(control.val()));
            break;
        default:
            if (control.is("[data-hapi-type=file-upload]")) {
                x = { file: control.attr('data-hapi-file-upload-url') || '' };
            }
            break;
        }

        console.log("Set", control.attr('data-hapi-path'), ' to ', x);
        if ( x !== null )
            eval("o." + control.attr('data-hapi-path') + ' = x;');
    });

    /* Send request */

    var method = form.attr('data-hapi-method');
    switch ( method ) {
    case 'PUTSETTINGS':
        var request = new XMLHttpRequest();
        request.open('PUT', "", true);
        request.setRequestHeader("Content-type", "application/json");

        request.onreadystatechange = function () {
            if ( request.readyState == 4 ) {
                if ( request.status >= 200 && request.status < 300 )
                    location.reload();
                else
                    alert('Error saving settings');
            }
        };

        request.send(JSON.stringify(o));
        break;
    case 'HTMLPOST':
        var input = $('<input type="hidden" name="POST_DATA"/>');
        var htmlForm = $('<form method="POST"></form>');
        htmlForm.append(input);
        input.val(btoa(unescape(encodeURIComponent(JSON.stringify(o))))
                  .replace(/\+/g, '-')
                  .replace(/\//g, '_')
                  .replace(/=/g, '*'));
        $(document.body).append(htmlForm);
        htmlForm.submit();
        break;
    default:
        alert('Unknown form method: ' + method);
    }
};

var hapiFormify = function (f) {
    var randomString = function (bytes) {
        var ret = '', bytesLeft = bytes;

        while ( bytesLeft > 0 ) {
            var a = Math.random().toString(36).substr(2);
            ret += a;
            bytesLeft -= a.length;
        }

        return ret.substr(0, ret.length + bytesLeft);
    };

    f.find('[data-hapi-type=file-upload]').each(function () {

        var fileName = $(this).attr("data-hapi-file-upload-name"),
            uri = $(this).attr("data-hapi-file-upload-url"),

            oldFileName = fileName,
            oldUri = uri,

            uploadUrl = $(this).attr("data-hapi-file-upload-backend-url"),
            uploadIcon = $('<button><i class="glyphicon glyphicon-cloud-upload"></i></button>'),
            fileInput = $('<input type="file" style="display: none"/>'),
            progress = $('<div class="progress"><div class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div></div>'),
            file = $('<div class="hapi-file-upload-file"><a class="hapi-file-upload-name"></a></div>'),
            errorEl = $('<div class="alert alert-danger"></div>'),
            status = 'waiting',
            request = null,
            maxSz = null, curSz = 0,

            fileUploadInput = $(this),

            updateUI = function () {
                if ( uri )
                    fileUploadInput.attr('data-hapi-file-upload-url', uri);
                else
                    fileUploadInput.removeAttr('data-hapi-file-upload-url');

                if ( fileName ) {
                    fileUploadInput.attr('data-hapi-file-upload-name', fileName);
                    switch ( status ) {
                    default:
                    case 'error':
                    case 'waiting':
                        file.find('.hapi-file-upload-name').text(fileName).css('font-style', 'normal');
                        progress.hide();
                        if (status == 'error') {
                            errorEl.show();
                        } else {
                            errorEl.hide();
                        }
                        break;
                    case 'uploading':
                        errorEl.hide();
                        progress.show();
                        file.find('.hapi-file-upload-name').text("Uploading " + fileName).css('font-style', 'normal');

                        if ( maxSz !== null ) {
                            var progressMeasure = curSz / maxSz;
                            progress.find('.progress-bar')
                                .css('width', progressMeasure*100 + '%')
                                .attr('aria-valuemax', '100')
                                .attr('aria-valuenow', ''+progressMeasure)
                                .removeClass('progress-bar-animated progress-bar-striped');
                        } else {
                            progress.find('.progress-bar')
                                .css('width', '100%')
                                .attr('aria-valuemax', '0')
                                .attr('aria-valuenow', '0')
                                .addClass('progress-bar-animated progress-bar-striped');
                        }
                        break;
                    }
                } else {
                    fileUploadInput.removeAttr('data-hapi-file-upload-name');
                    errorEl.hide();
                    progress.hide();
                    file.find('.hapi-file-upload-name').text("No File Uploaded").attr('href', '#').css('font-style', 'italic');
                }
            },

            startRequest = function (file) {
                var fd = new FormData();
                request = new XMLHttpRequest();
                request.open("POST", uploadUrl, true);
                maxSz = file.size;
                curSz = 0;

                request.onreadystatechange = function () {
                    if ( request.readyState == 4 ) {
                        if ( request.status == 201 ) {
                            var resp = JSON.parse(request.responseText);
                            oldFileName = fileName = resp.fileName;
                            oldUri = uri = resp.uri;
                            status = 'waiting';
                            fileInput.prop("disabled", false);
                            updateUI();
                            request = null;
                        } else {
                            status = 'error';
                            fileName = oldFileName;
                            uri = oldUri;
                            status = 'error';
                            errorEl.text(request.responseText);
                            fileInput.prop("disabled", false);
                            request = null;
                        }
                    }
                };

                request.upload.addEventListener('progress', function (oEvent) {
                    if ( oEvent.lengthComputable ) {
                        curSz = oEvent.loaded;
                        maxSz = oEvent.total;
                    } else {
                        maxSz = null;
                    }
                    updateUI();
                });

                status = 'uploading';
                fileInput.prop("disabled", true);
                updateUI();

                fd.append('file', file);
                fd.append('secret', randomString(256));
                request.send(fd);
            };

        updateUI();

        $(this).append(errorEl).append(uploadIcon).append(file).append(progress).append(fileInput);

        uploadIcon.on('click', function (e) {
            fileInput.click();
            e.preventDefault();
        });
        $(fileInput).on('change', function () {
            console.log('on file change');
            var ctl = $(this).get()[0];
            if ( ctl.files.length > 0 ) {
                var file = ctl.files[0];
                fileName = file.name;

                startRequest(file);
            }
        });
    });
};

$(document).ready(function () {
    $('form[data-hapi-form=true]').each(function () { hapiFormify($(this)); });
});
