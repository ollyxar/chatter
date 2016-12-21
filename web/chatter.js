/**
 * Chatter example js
 *
 * @author Olexy Sviridenko (alexslipknot(at)europe.com)
 * 2016-12-21
 * @license MIT
 */

var offsetMM = new Date().getTimezoneOffset(),
    login = 'None',
    avatar = '',
    userId = 0,
    smiles = [];

function chatInit() {
    login = $('#login').val();
    avatar = $('#avatar').val();
    userId = ~~$('#user_id').val();

    $.get('smiles.json', function (data) {
        var html = '';
        smiles = data.smiles;
        for (var smile_i in smiles) {
            html += '<i class="emoji icon-' + smiles[smile_i].value + '" data-key="' + smiles[smile_i].key + '"></i>';
        }
        $('.smiles-block').html(html);
    });

    $('.no-chatt-item').hide().parents('#chatt').removeClass('no-chatt');
    $('.logout-chatt').show();

    pull();
}

$('#chatt').on('click', '.no-chatt-item', function () {
    chatInit()
});

$('.logout-chatt').click(function () {
    $('#chatt').addClass('no-chatt');
    $('.logout-chatt').hide();
    $('#chatt ul').html('<li class="no-chatt-item btn-primary">Enter</li>');
    $('.no-chatt-item').show();
});


$('#chatt button[type=submit]').click(function (e) {
    e.preventDefault();

    var $this = $(this);

    if (!$this.is(':disabled')) {
        $this.attr('disabled', 'disabled');

        $.ajax({
            url: 'post.php',
            type: 'post',
            data: 'post=' + encodeURIComponent($('#c_post').val())
            + '&user_id=' + userId
            + '&avatar=' + encodeURIComponent(avatar)
            + '&name=' + login,
            complete: function () {
                $this.removeAttr('disabled');
            },
            success: function (data) {
                if (data) {
                    $('#c_post').val('');
                }
            }
        });
    }
});

$('#c_post').keyup(function (e) {
    if (e.keyCode == 13) {
        $('#chatt button[type=submit]').trigger('click');
    }
});


function pull() {
    $.ajax({
        url: 'http://127.0.0.1:2052/pull',
        type: 'get',
        dataType: 'json',
        data: 'message_id=' + ($('#chatt ul li:last').data('id') || -1),
        success: function (data) {
            if (data.Posts && !$('#chatt').hasClass('no-chatt')) {
                var i = $('#chatt ul li:last').data('id') || -1;

                $.each(data.Posts, function () {
                    var post_time = new Date('December 29, 1989 ' + this.time + ':00');

                    post_time.setTime(post_time.getTime() - offsetMM * 60 * 1000);

                    if ($('li.chat-item[data-id="' + this.uniqueId + '"]').length == 0) {
                        var html = '';
                        html += '<li class="chat-item" data-id="' + this.uniqueId + '" data-user="' + this.userId + '">';
                        html += '<img src="' + this.avatar + '" alt="">';
                        html += '<a target="_blank" href="/users/id' + this.userId + '" class="caption">' + this.name + '</a>';
                        html += '<p class="item-time">' + post_time.getHours() + ':' + (post_time.getMinutes() < 10 ? '0' + post_time.getMinutes() : post_time.getMinutes()) + '</p>';

                        var pm = this.post;

                        for (var i in smiles) {
                            var replaceMask = '<i class="emoji icon-' + smiles[i].value + '"></i>';
                            pm = pm.replace(/smiles[i].key/ig, replaceMask);
                        }

                        html += '<div class="post">' + pm + '</div>';
                        html += '</li>';

                        var must_scroll = $('.chatt')[0].scrollTop + 20 >= $('.chatt')[0].scrollHeight - $('.chatt')[0].clientHeight;
                        $('#chatt ul').append(html);

                        if (must_scroll) {
                            autoscrollChat();
                        }
                    }
                });

            }
        },
        complete: function () {
            if (!$('#chatt').hasClass('no-chatt')) {
                setTimeout(function () {
                    pull();
                }, 100);
            }
        }
    });
}

function autoscrollChat() {
    $("#chatt ul").scrollTop($("#chatt ul")[0].scrollHeight);
}