package initializer;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;

public class IndexEntryInitializer {
    public static void main(String... args) {
        SwingUtilities.invokeLater(() -> {
            var i = new IndexEntryInitializer();
            i.init();
        });
    }

    void init() {
        var data = new ArrayList<List<String>>();
        var frame = new JFrame();
        var contentPane = new JPanel();
        var layout = new BoxLayout(contentPane, BoxLayout.Y_AXIS);
        contentPane.setLayout(layout);
        frame.add(new JScrollPane(contentPane));
        var add = new JButton("Add");
        add.addActionListener(a -> {
            var datum = Arrays.asList("", "value", "");
            data.add(datum);
            var cmp = createEntryBlock(datum);
            contentPane.add(cmp, 1);
            var strut = Box.createVerticalStrut(8);
            contentPane.add(strut, 1);
            frame.revalidate();
            frame.repaint();
        });
        var showSexpr = new JButton("Show sexpr");
        showSexpr.addActionListener(a -> {
            var text = buildSexprText(data);
            var tf = new JTextArea();
            tf.setText(text);
            var f = new JFrame();
            f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            f.add(new JScrollPane(tf));
            f.setSize(1600, 900);
            f.setVisible(true);
        });
        var btnHolder = new JPanel();
        btnHolder.add(add);
        btnHolder.add(showSexpr);
        contentPane.add(btnHolder);
        contentPane.add(Box.createVerticalGlue());

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(1600, 900);
        frame.setVisible(true);
    }

    String buildSexprText(List<List<String>> data) {
        var sb = new StringBuilder();
        var first = true;
        sb.append("(");
        for (var datum: data) {
            if (!first) {
                sb.append("\n ");
            } else {
                first = false;
            }
            sb.append("((name . \"");
            sb.append(datum.get(0).trim());
            sb.append("\")\n");
            sb.append("  (signature ");
            switch (datum.get(1)) {
                case "value": sb.append("value"); break;
                case "lambda": sb.append("lambda () undefined"); break;
                case "case-lambda": sb.append("case-lambda (() undefined)"); break;
                case "syntax-rules": sb.append("syntax-rules () ((_ ))"); break;
            }
            sb.append(")\n");
            sb.append("  (desc . \"");
            sb.append(escape(datum.get(2).trim()));
            sb.append("\"))");
        }
        sb.append(")");
        return sb.toString();
    }

    String escape(String s) {
        return s.replace("\\", "\\\\")
            .replace("\"", "\\\"");
    }

    JComponent createEntryBlock(List<String> data) {
        var p = new JPanel() {
            @Override
            public Dimension getMaximumSize() {
                return new Dimension(960, getPreferredSize().width);
            }
        };
        var layout = new GridBagLayout();
        GridBagConstraints c;
        p.setLayout(layout);
        p.setBorder(BorderFactory.createLineBorder(Color.black));

        c = new GridBagConstraints();
        c.gridx = 0;
        c.gridy = 0;
        c.anchor = GridBagConstraints.LINE_START;
        c.insets = new Insets(4, 4, 4, 4);
        p.add(new JLabel("Name"), c);

        c = new GridBagConstraints();
        c.gridx = 1;
        c.gridy = 0;
        c.weightx = 1;
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(4, 4, 4, 4);
        var name = new JTextField();
        addChangeListener(name, text -> data.set(0, text));
        p.add(name, c);

        c = new GridBagConstraints();
        c.gridx = 0;
        c.gridy = 1;
        c.anchor = GridBagConstraints.LINE_START;
        c.insets = new Insets(4, 4, 4, 4);
        p.add(new JLabel("Type"), c);

        c = new GridBagConstraints();
        c.gridx = 1;
        c.gridy = 1;
        c.weightx = 1;
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(4, 4, 4, 4);
        var type = new JComboBox<String>(new String[]{ "value", "lambda", "case-lambda", "syntax-rules" });
        type.addItemListener(l -> data.set(1, type.getSelectedItem().toString()));
        p.add(type, c);

        c = new GridBagConstraints();
        c.gridx = 0;
        c.gridy = 2;
        c.anchor = GridBagConstraints.LINE_START;
        c.insets = new Insets(4, 4, 4, 4);
        p.add(new JLabel("Description"), c);

        c = new GridBagConstraints();
        c.gridx = 0;
        c.gridy = 3;
        c.gridwidth = 2;
        c.weightx = 1;
        c.weighty = 1;
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(4, 4, 4, 4);
        var desc = new JTextArea();
        var scroll = new JScrollPane(desc);
        scroll.setPreferredSize(new Dimension(500, 200));
        addChangeListener(desc, text -> data.set(2, text));
        p.add(scroll, c);

        return p;
    }

    void addChangeListener(JTextComponent tf, Consumer<String> handler) {
        var listener = new DocumentListener() {
			@Override
			public void insertUpdate(DocumentEvent e) {
                handler.accept(tf.getText());
			}

			@Override
			public void removeUpdate(DocumentEvent e) {
                handler.accept(tf.getText());
			}

			@Override
			public void changedUpdate(DocumentEvent e) {
                handler.accept(tf.getText());
			}
        };
        tf.getDocument().addDocumentListener(listener);
    }

}
