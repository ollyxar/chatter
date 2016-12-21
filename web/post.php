<?php

/**
 * Example of middleware for posing message
 * Please provide your own authorize method because this is just an example!
 * Don't forget to enable cURL extension!
 */

if (isset($_POST['post'])) {
    $response_text = '';
    $post = htmlspecialchars(trim($_POST['post']));

    if ($post) {
        try {
            $ch = curl_init();

            if ($ch === false) {
                throw new Exception('failed to initialize cURL. Did you forget to turn on extension?');
            }

            curl_setopt($ch, CURLOPT_URL, 'http://127.0.0.1:2052/post');
            curl_setopt($ch, CURLOPT_POST, 1);

            curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

            curl_setopt($ch, CURLOPT_POSTFIELDS, http_build_query([
                'post'   => $post,
                //todo: It is very important to verify user here. Do not trust POST data!
                'name'   => htmlspecialchars(trim($_POST['name']), ENT_QUOTES),
                'avatar' => htmlspecialchars(trim($_POST['avatar']), ENT_QUOTES),
                'userId' => htmlspecialchars(trim($_POST['user_id']), ENT_QUOTES),
                'secret' => 'qwerty123'
            ]));

            $response_text = curl_exec($ch);

            if ($response_text === false) {
                throw new Exception(curl_error($ch), curl_errno($ch));
            }

            curl_close($ch);
        } catch (Exception $e) {
            trigger_error(sprintf('Curl failed with error #%d: %s', $e->getCode(), $e->getMessage()), E_USER_ERROR);
        }
    }

    die($response_text);
}